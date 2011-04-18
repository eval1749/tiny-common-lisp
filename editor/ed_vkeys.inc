// -*- Mode: C++; -*-

#if !defined(KeyEntry)
    #define KeyEntry(mp_name, mp_pname
#endif

    KeyEntry(0, 0),         // 0x00
    KeyEntry(0, LBUTTON),   // 0x01
    KeyEntry(0, RBUTTON),   // 0x02
    KeyEntry(0, CANCEL),    // 0x03
    KeyEntry(0, MBUTTON),   // 0x04    /* NOT contiguous with L & RBUTTON */
    KeyEntry(0, XBUTTON1),  // 0x05    /* NOT contiguous with L & RBUTTON */
    KeyEntry(0, XBUTTON2),  // 0x06    /* NOT contiguous with L & RBUTTON */
    KeyEntry(0, 07),
    KeyEntry(Key_Backspace, Backspace), // 0x08 BACK
    KeyEntry(Key_Tab, Tab),       // 0x09
    KeyEntry(0, 0A),
    KeyEntry(0, 0B),
    KeyEntry(0, Clear),     // 0x0C
    KeyEntry(Key_Enter, Enter),     // 0x0D RETURN
    KeyEntry(0, 0E)
    KeyEntry(0, 0F),

    KeyEntry(0, SHIFT),     // 0x10
    KeyEntry(0, CONTROL),   // 0x11
    KeyEntry(0, MENU),      // 0x12
    KeyEntry(0, Pause),     // 0x13
    KeyEntry(0, Capital),   // 0x14
    KeyEntry(0, KANA),      // 0x15 HANGEUL/NAGUL
    KeyEntry(0, 16),
    KeyEntry(0, JUNJA),     // 0x17
    KeyEntry(0, FINAL),     // 0x18
    KeyEntry(0, KANJI),     // 0x19 HANJA
    KeyEntry(0, 1A),
    KeyEntry(Key_Escape, Escape),    // 0x1B
    KeyEntry(0, CONVERT),   // 0x1C
    KeyEntry(0, NONCONVERT),// 0x1D
    KeyEntry(0, ACCEPT),    // 0x1E
    KeyEntry(0, MODECHANGE),// 0x1F

    KeyEntry(0, Space),     // 0x20
    KeyEntry(Key_PageUp, PageUp),    // 0x21 PRIOR
    KeyEntry(Key_PageDown, PageDn),    // 0x22 NEXT
    KeyEntry(Key_End, End),       // 0x23
    KeyEntry(Key_Home, Home),      // 0x24
    KeyEntry(Key_Left, Left),      // 0x25
    KeyEntry(Key_Up, Up),        // 0x26
    KeyEntry(Key_Right, Right),     // 0x27
    KeyEntry(Key_Down, Down),      // 0x28
    KeyEntry(0, Select),    // 0x29
    KeyEntry(0, Print),     // 0x2A
    KeyEntry(0, EXECUTE),   // 0x2B
    KeyEntry(0, SNAPSHOT),  // 0x2C
    KeyEntry(Key_Insert, Insert),    // 0x2D
    KeyEntry(Key_Delete, Delete),    // 0x2E
    KeyEntry(0, Help),      // 0x2F

    KeyEntry(0, 30), KeyEntry(0, 31), KeyEntry(0, 32), KeyEntry(0, 33),
    KeyEntry(0, 34), KeyEntry(0, 35), KeyEntry(0, 36), KeyEntry(0, 37),
    KeyEntry(0, 38), KeyEntry(0, 39), KeyEntry(0, 3A), KeyEntry(0, 3B),
    KeyEntry(0, 3C), KeyEntry(0, 3D), KeyEntry(0, 3E), KeyEntry(0, 3F),

    KeyEntry(0, 40), KeyEntry(0, 41), KeyEntry(0, 42), KeyEntry(0, 43),
    KeyEntry(0, 44), KeyEntry(0, 45), KeyEntry(0, 46), KeyEntry(0, 47),
    KeyEntry(0, 48), KeyEntry(0, 49), KeyEntry(0, 4A), KeyEntry(0, 4B),
    KeyEntry(0, 4C), KeyEntry(0, 4D), KeyEntry(0, 4E), KeyEntry(0, 4F),

    KeyEntry(0, 50), KeyEntry(0, 51), KeyEntry(0, 52), KeyEntry(0, 53),
    KeyEntry(0, 54), KeyEntry(0, 55), KeyEntry(0, 56), KeyEntry(0, 57),
    KeyEntry(0, 58), KeyEntry(0, 59), KeyEntry(0, 5A),
    KeyEntry(0, LWIN),      // 0x5B
    KeyEntry(0, RWIN),      // 0x5C
    KeyEntry(0, APPS),      // 0x5D
    KeyEntry(0, 5E),
    KeyEntry(0, SLEEP),     // 0x5F

    KeyEntry(0, NumPad0),   // 0x60
    KeyEntry(0, NumPad1),   // 0x61
    KeyEntry(0, NumPad2),   // 0x62
    KeyEntry(0, NumPad3),   // 0x63
    KeyEntry(0, NumPad4),   // 0x64
    KeyEntry(0, NumPad5),   // 0x65
    KeyEntry(0, NumPad6),   // 0x66
    KeyEntry(0, NumPad7),   // 0x67
    KeyEntry(0, NumPad8),   // 0x68
    KeyEntry(0, NumPad9),   // 0x69
    KeyEntry(0, MULTIPLY),  // 0x6A
    KeyEntry(0, ADD),       // 0x6B
    KeyEntry(0, SEPARATOR), // 0x6C
    KeyEntry(0, SUBTRACT),  // 0x6D
    KeyEntry(0, DECIMAL),   // 0x6E
    KeyEntry(0, DIVIDE),    // 0x6F

    KeyEntry(Key_F1, F1),    // 0x70
    KeyEntry(Key_F2, F2),    // 0x71
    KeyEntry(Key_F3, F3),    // 0x72
    KeyEntry(Key_F4, F4),    // 0x73
    KeyEntry(Key_F5, F5),    // 0x74
    KeyEntry(Key_F6, F6),    // 0x75
    KeyEntry(Key_F7, F7),    // 0x76
    KeyEntry(Key_F8, F8),    // 0x77
    KeyEntry(Key_F9, F9),    // 0x78
    KeyEntry(Key_F10, F10),   // 0x79
    KeyEntry(Key_F11, F11),   // 0x7A
    KeyEntry(Key_F12, F12),   // 0x7B
    KeyEntry(Key_F13, F13),   // 0x7C
    KeyEntry(Key_F14, F14),   // 0x7D
    KeyEntry(Key_F15, F15),   // 0x7E
    KeyEntry(Key_F16, F16),   // 0x7F
    KeyEntry(Key_F17, F17),   // 0x80
    KeyEntry(Key_F18, F18),   // 0x81
    KeyEntry(Key_F19, F19),   // 0x82
    KeyEntry(Key_F20, F20),   // 0x83
    KeyEntry(Key_F21, F21),   // 0x84
    KeyEntry(Key_F22, F22),   // 0x85
    KeyEntry(Key_F23, F23),   // 0x86
    KeyEntry(Key_F24, F24),   // 0x87

    KeyEntry(0, 88), KeyEntry(0, 89), KeyEntry(0, 8A), KeyEntry(0, 8B),
    KeyEntry(0, 8C), KeyEntry(0, 8D), KeyEntry(0, 8E), KeyEntry(0, 8F),

    KeyEntry(0, NumLocK),   // 0x90
    KeyEntry(0, ScrLk),     // 0x91 SCROLL

    // NEC PC-9800 kbd definitions
    //KeyEntry(0, OEM_NEC_EQUAL), //  0x92   // '=' key on numpad

    // Fujitsu/OASYS kbd definitions
    KeyEntry(0, OEM_FJ_JISHO),  // 0x92   // 'Dictionary' key
    KeyEntry(0, OEM_FJ_MASSHOU),// 0x93   // 'Unregister word' key
    KeyEntry(0, OEM_FJ_TOUROKU),// 0x94   // 'Register word' key
    KeyEntry(0, OEM_FJ_LOYA),   // 0x95   // 'Left OYAYUBI' key
    KeyEntry(0, OEM_FJ_ROYA),   // 0x96   // 'Right OYAYUBI' key
    KeyEntry(0, 97),
    KeyEntry(0, 98), KeyEntry(0, 99), KeyEntry(0, 9A), KeyEntry(0, 9B),
    KeyEntry(0, 9C), KeyEntry(0, 9D), KeyEntry(0, 9E), KeyEntry(0, 9F),

    // VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
    // Used only as parameters to GetAsyncKeyState() and GetKeyState().
    // No other API or message will distinguish left and right keys in this
    // way.
    KeyEntry(0, LSHIFT),    // 0xA0
    KeyEntry(0, RSHIFT),    // 0xA1
    KeyEntry(0, LCONTROL),  // 0xA2
    KeyEntry(0, RCONTROL),  // 0xA3
    KeyEntry(0, LMENU),     // 0xA4
    KeyEntry(0, RMENU),     // 0xA5
    KeyEntry(0, BROWSER_BACK),      // 0xA6
    KeyEntry(0, BROWSER_FORWARD),   // 0xA7
    KeyEntry(0, BROWSER_REFRESH),   // 0xA8
    KeyEntry(0, BROWSER_STOP),      // 0xA9
    KeyEntry(0, BROWSER_SEARCH),    // 0xAA
    KeyEntry(0, BROWSER_FAVORITES), // 0xAB
    KeyEntry(0, BROWSER_HOME),      // 0xAC
    KeyEntry(0, VOLUME_MUTE),       // 0xAD
    KeyEntry(0, VOLUME_DOWN),       // 0xAE
    KeyEntry(0, VOLUME_UP),         // 0xAF

    KeyEntry(0, MEDIA_NEXT_TRACK),  // 0xB0
    KeyEntry(0, MEDIA_PREV_TRACK),  // 0xB1
    KeyEntry(0, MEDIA_STOP),        // 0xB2
    KeyEntry(0, MEDIA_PLAY_PAUSE),  // 0xB3
    KeyEntry(0, LAUNCH_MAIL),       // 0xB4
    KeyEntry(0, LAUNCH_MEDIA_SELECT), // 0xB5
    KeyEntry(0, LAUNCH_APP1),       // 0xB6
    KeyEntry(0, LAUNCH_APP2),       // 0xB7
    KeyEntry(0, B8), KeyEntry(0, B9),
    KeyEntry(0, OEM_1),         // 0xBA   // ';:' for US
    KeyEntry(0, OEM_PLUS),      // 0xBB   // '+' any country
    KeyEntry(0, OEM_COMMA),     // 0xBC   // ',' any country
    KeyEntry(0, OEM_MINUS),     // 0xBD   // '-' any country
    KeyEntry(0, OEM_PERIOD),    // 0xBE   // '.' any country
    KeyEntry(0, OEM_2),         // 0xBF   // '/?' for US

    KeyEntry(0, OEM_3),         // 0xC0   // '`~' for US
                     KeyEntry(0, C1), KeyEntry(0, C2), KeyEntry(0, C3),
    KeyEntry(0, C4), KeyEntry(0, C5), KeyEntry(0, C6), KeyEntry(0, C7),
    KeyEntry(0, C8), KeyEntry(0, C9), KeyEntry(0, CA), KeyEntry(0, CB),
    KeyEntry(0, CC), KeyEntry(0, CD), KeyEntry(0, CE), KeyEntry(0, CF),

    KeyEntry(0, D0), KeyEntry(0, D1), KeyEntry(0, D2), KeyEntry(0, D3),
    KeyEntry(0, D4), KeyEntry(0, D5), KeyEntry(0, D6), KeyEntry(0, D7),
    KeyEntry(0, D8), KeyEntry(0, D9), KeyEntry(0, DA),
    KeyEntry(0, OEM_4), // 0xDB  //  '[{' for US
    KeyEntry(0, OEM_5), // 0xDC  //  '\|' for US
    KeyEntry(0, OEM_6), // 0xDD  //  ']}' for US
    KeyEntry(0, OEM_7), // 0xDE  //  ''"' for US
    KeyEntry(0, OEM_8), // 0xDF

    KeyEntry(0, E0),
    KeyEntry(0, OEM_AX),    // 0xE1  //  'AX' key on Japanese AX kbd
    KeyEntry(0, OEM_102),   // 0xE2  //  "<>" or "\|" on RT 102-key kbd.
    KeyEntry(0, ICO_HELP),  // 0xE3  //  Help key on ICO
    KeyEntry(0, ICO_00),    // 0xE4  //  00 key on ICO
    KeyEntry(0, PROCESSKEY), // 0xE5
    KeyEntry(0, ICO_CLEAR), // 0xE6
    KeyEntry(0, PACKET),    // 0xE7
    KeyEntry(0, E8),
    // Nokia/Ericsson definitions
    KeyEntry(0, OEM_RESET), // 0xE9
    KeyEntry(0, OEM_JUMP),  // 0xEA
    KeyEntry(0, OEM_PA1),   // 0xEB
    KeyEntry(0, OEM_PA2),   // 0xEC
    KeyEntry(0, OEM_PA3),   // 0xED
    KeyEntry(0, OEM_WSCTRL),// 0xEE
    KeyEntry(0, OEM_CUSEL), // 0xEF

    KeyEntry(0, OEM_ATTN),  // 0xF0
    KeyEntry(0, OEM_FINISH),// 0xF1
    KeyEntry(0, OEM_COPY),  // 0xF2
    KeyEntry(0, OEM_AUTO),  // 0xF3
    KeyEntry(0, OEM_ENLW),  // 0xF4
    KeyEntry(0, OEM_BACKTAB),//0xF5
    KeyEntry(0, ATTN),      // 0xF6
    KeyEntry(0, CRSEL),     // 0xF7
    KeyEntry(0, EXSEL),     // 0xF8
    KeyEntry(0, EREOF),     // 0xF9
    KeyEntry(0, PLAY),      // 0xFA
    KeyEntry(0, ZOOM),      // 0xFB
    KeyEntry(0, NONAME),    // 0xFC
    KeyEntry(0, PA1),       // 0xFD
    KeyEntry(0, OEM_CLEAR), // 0xFE
    KeyEntry(0, FF),

#undef KeyEntry
