MODULE GITS;

FROM SYSTEM  IMPORT ADR, ADDRESS, TSIZE;
FROM Strings IMPORT Length, CompareStr, Copy;
FROM FIO     IMPORT File, Exists, OpenToRead, Close, IsNoError, ReadChar, WasEOLN, EOF;
FROM BitByteOps IMPORT ByteAnd;
IMPORT Args; (* IsNoError, IsActive, ReadNBytes ReadChar ReadString EOLN *)
FROM RandomNumber IMPORT RandomInit, RandomCard, RandomInt;
FROM ncurses IMPORT printf, scanf, curs_set, endwin, initscr, refresh, move, addstr, mvaddstr,
                    napms, clear, nodelay, getch, keypad, setlocale, has_colors, start_color,
                    init_pair, bkgd, COLOR_PAIR, getmaxx, getmaxy, addch, addnstr, mvaddnstr;


(* #TODO Move these to ncurses definition module *)
CONST
    ERR    = -1;
    LC_ALL = 6;
    OK     = 0;
    BLACK  = 0;
    GREEN  = 2;
    CHAR0  = 48;
    CHAR9  = 57;
    MAX_MSGS = 16;
    BUFFER_SIZE = 64;

TYPE
    State = (Init, Middle, End);
    (* String = ARRAY [0..BUFFER_SIZE] OF CHAR; *)

VAR
    (* General variables *)
    msg : ARRAY [0..BUFFER_SIZE] OF CHAR;
    messages : ARRAY [0..MAX_MSGS] OF ARRAY [0..BUFFER_SIZE] OF CHAR;
    messages_count : INTEGER;
    current_msg : INTEGER;
    i, j, width, height : INTEGER;
    key, frame_count : INTEGER;
    window : ADDRESS;
    quit   : BOOLEAN;
    dummy  : BOOLEAN;
    char   : CARDINAL;
    state  : State;

    (* Middle State Variables *)
    draw_probability, delta, chance : INTEGER;

    (* End State Variables *)
    msg_len, msg_half, msg_lhs_len, msgx_delta : INTEGER;
    msg_lhs_byte_len, msg_byte_len : INTEGER;
    msgx, msgy : INTEGER;
    msg_mid_address : ADDRESS;


PROCEDURE AddMessage(s: ARRAY OF CHAR);
    VAR l : INTEGER;
BEGIN
    l := Length(s);
    Copy(s, 0, l, messages[messages_count]);
    INC(messages_count);
END AddMessage;


PROCEDURE LoadDefaultMessages;
BEGIN
    AddMessage("Ghost In The Shell");
    AddMessage("Based on the manga by SHIROW MASAMUNE");
    AddMessage("Screenplay by KAZUNORI ITO");
    AddMessage("Character design by HIROYUKI OKIURA");
    AddMessage("Weapon design by MITSUO ISO");
    AddMessage("Music by KENJI KAWAI");
END LoadDefaultMessages;

PROCEDURE LoadMessagesFromFile(path: ARRAY OF CHAR);
VAR
    c       : CHAR;
    file    : File;
    str_len : INTEGER;
BEGIN
    file := OpenToRead(path);
    str_len := 0;

    WHILE NOT EOF(file) AND (messages_count < MAX_MSGS) DO
        c := ReadChar(file);
        IF VAL(CARDINAL, c) = 10 THEN (* 10 = newline on unix systems, newline on windows is 2 chars: 13 and 10 *)
            messages[messages_count, str_len] := VAL(CHAR, 0);
            INC(messages_count);
            str_len := 0;
        ELSE
            messages[messages_count, str_len] := c;
            INC(str_len);
        END;
    END;
    
    Close(file);
END LoadMessagesFromFile;

PROCEDURE ByteAndCodepointLen(s : ARRAY OF CHAR; VAR bl : INTEGER; VAR cl : INTEGER);
    VAR index : INTEGER;
BEGIN
    bl := Length(s);
    cl := 0;
    index := 0;
    
    WHILE index < bl DO
        IF 0F0H = ByteAnd(0F8H, s[index]) THEN
            INC(index, 4);
        ELSIF 0E0H = ByteAnd(0F0H, s[index]) THEN
            INC(index, 3);
        ELSIF 0C0H = ByteAnd(0E0H, s[index]) THEN
            INC(index, 2);
        ELSE
            INC(index);
        END;
        INC(cl);
    END;
END ByteAndCodepointLen;

PROCEDURE StringMiddle(VAR s : ARRAY OF CHAR; bytelen, utf8len : INTEGER; VAR index : INTEGER) : ADDRESS;
VAR
    adr : ADDRESS;
    mid, codepoint_count : INTEGER;
BEGIN
    index := 0;
    mid := utf8len DIV 2;
    codepoint_count := 0;

    WHILE index < bytelen DO
        IF codepoint_count = mid THEN RETURN ADR(s[index]) END;
        
        IF 0F0H = ByteAnd(0F8H, s[index]) THEN
            INC(index, 4);
        ELSIF 0E0H = ByteAnd(0F0H, s[index]) THEN
            INC(index, 3);
        ELSIF 0C0H = ByteAnd(0E0H, s[index]) THEN
            INC(index, 2);
        ELSE
            INC(index);
        END;
        INC(codepoint_count);
    END;
    RETURN NIL;
END StringMiddle;

(* Entry Point *)
BEGIN
    
    messages_count := 0;
    current_msg := 0;
    quit := FALSE;
    i := 0;
    j := 0;
    state := Init;
    frame_count := 0;
    draw_probability := 100;
    delta := 1;
    msgx_delta := 10;

    IF Args.Narg() > 2 THEN
        dummy := Args.GetArg(msg, 1);
        IF CompareStr(msg, "msgs") = 0 THEN
            FOR i := 2 TO Args.Narg() - 1 DO
                dummy := Args.GetArg(messages[messages_count], i);
                INC(messages_count);
            END;
            
        ELSIF CompareStr(msg, "file") = 0 THEN
            dummy := Args.GetArg(msg, 2);
            IF Exists(msg) THEN
                LoadMessagesFromFile(msg);
            ELSE
                LoadDefaultMessages();
            END;
            
        ELSE
            LoadDefaultMessages();
        END;
    ELSE
        LoadDefaultMessages();
    END;

    setlocale(LC_ALL, "");
    window := initscr();
    curs_set(0);
    nodelay(window, TRUE);
    keypad(window, TRUE);

    IF has_colors() AND (start_color() = OK) THEN
        init_pair(1, GREEN, BLACK);
        bkgd(VAL(CARDINAL, COLOR_PAIR(1)));
    END;

    RandomInit(987654321);

    WHILE NOT quit DO
        width := getmaxx(window);
        height := getmaxy(window);

        IF state = Init THEN
            FOR j := 0 TO height DO
                FOR i := 0 TO width DO
                    move(j, i);
                    char := RandomCard(CHAR0, CHAR9);
                    addch(char);
                END;
            END;

            frame_count := frame_count + 1;
            IF frame_count >= 30 THEN
                frame_count := 0;
                state := Middle;
            END;
            
        ELSIF state = Middle THEN
            FOR j := 0 TO height DO
                FOR i := 0 TO width DO
                    chance := RandomInt(0, 100);
                    IF draw_probability > chance THEN
                        move(j, i);
                        char := RandomCard(48, 57);
                        addch(char);
                    END;
                    
                END;
            END;

            draw_probability := draw_probability - delta;
            IF draw_probability <= 0 THEN
                draw_probability := 100;
                delta := 1;
                frame_count := 0;
                state := End;
                    
                (* msg_len := Length(messages[current_msg]); *)
                (* msg_half := msg_len DIV 2; *)
                (* msg_lhs_len := msg_len - msg_half; *)
                (* msg_mid_address := ADR(messages[current_msg]) + (VAL(CARDINAL, msg_half) * TSIZE(CHAR)); *)

                ByteAndCodepointLen(messages[current_msg], msg_byte_len, msg_len);
                msg_half := msg_len DIV 2;
                msg_lhs_len := msg_len - msg_half;
                msg_mid_address := StringMiddle(messages[current_msg], msg_byte_len, msg_len, msg_lhs_byte_len);

            ELSIF draw_probability < 50 THEN
                delta := 2;
            END;
            
        ELSIF state = End THEN
            msgx := (width - msg_len) DIV 2;
            msgy := height DIV 2;

            IF msgx_delta > 0 THEN DEC(msgx_delta, 2); END;
            
            mvaddnstr(msgy, msgx - msgx_delta, ADR(messages[current_msg]), msg_byte_len);
            mvaddnstr(msgy, msgx + msgx_delta + msg_half, msg_mid_address, -1);
            
            INC(frame_count);
            
            IF frame_count = 40 THEN
                msgx_delta := 10;
                state := Init;
                frame_count := 0;
                INC(current_msg);
                IF current_msg = messages_count THEN current_msg := 0; END;
            END;
            
        END;

        key := getch();
        IF key # ERR THEN
            quit := TRUE;
        END;
        
        refresh();
        napms(30);
        clear();
    END;

    endwin();
    
END GITS.

