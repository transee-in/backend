-module(win1251).
-export([decode/1]).

decode(Str) ->
  utf8:to_binary(lists:map(fun code/1, Str)).

code(16#00) -> 16#0000; %% null
code(16#01) -> 16#0001; %% start of heading
code(16#02) -> 16#0002; %% start of text
code(16#03) -> 16#0003; %% end of text
code(16#04) -> 16#0004; %% end of transmission
code(16#05) -> 16#0005; %% enquiry
code(16#06) -> 16#0006; %% acknowledge
code(16#07) -> 16#0007; %% bell
code(16#08) -> 16#0008; %% backspace
code(16#09) -> 16#0009; %% horizontal tabulation
code(16#0A) -> 16#000A; %% line feed
code(16#0B) -> 16#000B; %% vertical tabulation
code(16#0C) -> 16#000C; %% form feed
code(16#0D) -> 16#000D; %% carriage return
code(16#0E) -> 16#000E; %% shift out
code(16#0F) -> 16#000F; %% shift in
code(16#10) -> 16#0010; %% data link escape
code(16#11) -> 16#0011; %% device control one
code(16#12) -> 16#0012; %% device control two
code(16#13) -> 16#0013; %% device control three
code(16#14) -> 16#0014; %% device control four
code(16#15) -> 16#0015; %% negative acknowledge
code(16#16) -> 16#0016; %% synchronous idle
code(16#17) -> 16#0017; %% end of transmission block
code(16#18) -> 16#0018; %% cancel
code(16#19) -> 16#0019; %% end of medium
code(16#1A) -> 16#001A; %% substitute
code(16#1B) -> 16#001B; %% escape
code(16#1C) -> 16#001C; %% file separator
code(16#1D) -> 16#001D; %% group separator
code(16#1E) -> 16#001E; %% record separator
code(16#1F) -> 16#001F; %% unit separator
code(16#20) -> 16#0020; %% space
code(16#21) -> 16#0021; %% exclamation mark
code(16#22) -> 16#0022; %% quotation mark
code(16#23) -> 16#0023; %% number sign
code(16#24) -> 16#0024; %% dollar sign
code(16#25) -> 16#0025; %% percent sign
code(16#26) -> 16#0026; %% ampersand
code(16#27) -> 16#0027; %% apostrophe
code(16#28) -> 16#0028; %% left parenthesis
code(16#29) -> 16#0029; %% right parenthesis
code(16#2A) -> 16#002A; %% asterisk
code(16#2B) -> 16#002B; %% plus sign
code(16#2C) -> 16#002C; %% comma
code(16#2D) -> 16#002D; %% hyphen-minus
code(16#2E) -> 16#002E; %% full stop
code(16#2F) -> 16#002F; %% solidus
code(16#30) -> 16#0030; %% digit zero
code(16#31) -> 16#0031; %% digit one
code(16#32) -> 16#0032; %% digit two
code(16#33) -> 16#0033; %% digit three
code(16#34) -> 16#0034; %% digit four
code(16#35) -> 16#0035; %% digit five
code(16#36) -> 16#0036; %% digit six
code(16#37) -> 16#0037; %% digit seven
code(16#38) -> 16#0038; %% digit eight
code(16#39) -> 16#0039; %% digit nine
code(16#3A) -> 16#003A; %% colon
code(16#3B) -> 16#003B; %% semicolon
code(16#3C) -> 16#003C; %% less-than sign
code(16#3D) -> 16#003D; %% equals sign
code(16#3E) -> 16#003E; %% greater-than sign
code(16#3F) -> 16#003F; %% question mark
code(16#40) -> 16#0040; %% commercial at
code(16#41) -> 16#0041; %% latin capital letter a
code(16#42) -> 16#0042; %% latin capital letter b
code(16#43) -> 16#0043; %% latin capital letter c
code(16#44) -> 16#0044; %% latin capital letter d
code(16#45) -> 16#0045; %% latin capital letter e
code(16#46) -> 16#0046; %% latin capital letter f
code(16#47) -> 16#0047; %% latin capital letter g
code(16#48) -> 16#0048; %% latin capital letter h
code(16#49) -> 16#0049; %% latin capital letter i
code(16#4A) -> 16#004A; %% latin capital letter j
code(16#4B) -> 16#004B; %% latin capital letter k
code(16#4C) -> 16#004C; %% latin capital letter l
code(16#4D) -> 16#004D; %% latin capital letter m
code(16#4E) -> 16#004E; %% latin capital letter n
code(16#4F) -> 16#004F; %% latin capital letter o
code(16#50) -> 16#0050; %% latin capital letter p
code(16#51) -> 16#0051; %% latin capital letter q
code(16#52) -> 16#0052; %% latin capital letter r
code(16#53) -> 16#0053; %% latin capital letter s
code(16#54) -> 16#0054; %% latin capital letter t
code(16#55) -> 16#0055; %% latin capital letter u
code(16#56) -> 16#0056; %% latin capital letter v
code(16#57) -> 16#0057; %% latin capital letter w
code(16#58) -> 16#0058; %% latin capital letter x
code(16#59) -> 16#0059; %% latin capital letter y
code(16#5A) -> 16#005A; %% latin capital letter z
code(16#5B) -> 16#005B; %% left square bracket
code(16#5C) -> 16#005C; %% reverse solidus
code(16#5D) -> 16#005D; %% right square bracket
code(16#5E) -> 16#005E; %% circumflex accent
code(16#5F) -> 16#005F; %% low line
code(16#60) -> 16#0060; %% grave accent
code(16#61) -> 16#0061; %% latin small letter a
code(16#62) -> 16#0062; %% latin small letter b
code(16#63) -> 16#0063; %% latin small letter c
code(16#64) -> 16#0064; %% latin small letter d
code(16#65) -> 16#0065; %% latin small letter e
code(16#66) -> 16#0066; %% latin small letter f
code(16#67) -> 16#0067; %% latin small letter g
code(16#68) -> 16#0068; %% latin small letter h
code(16#69) -> 16#0069; %% latin small letter i
code(16#6A) -> 16#006A; %% latin small letter j
code(16#6B) -> 16#006B; %% latin small letter k
code(16#6C) -> 16#006C; %% latin small letter l
code(16#6D) -> 16#006D; %% latin small letter m
code(16#6E) -> 16#006E; %% latin small letter n
code(16#6F) -> 16#006F; %% latin small letter o
code(16#70) -> 16#0070; %% latin small letter p
code(16#71) -> 16#0071; %% latin small letter q
code(16#72) -> 16#0072; %% latin small letter r
code(16#73) -> 16#0073; %% latin small letter s
code(16#74) -> 16#0074; %% latin small letter t
code(16#75) -> 16#0075; %% latin small letter u
code(16#76) -> 16#0076; %% latin small letter v
code(16#77) -> 16#0077; %% latin small letter w
code(16#78) -> 16#0078; %% latin small letter x
code(16#79) -> 16#0079; %% latin small letter y
code(16#7A) -> 16#007A; %% latin small letter z
code(16#7B) -> 16#007B; %% left curly bracket
code(16#7C) -> 16#007C; %% vertical line
code(16#7D) -> 16#007D; %% right curly bracket
code(16#7E) -> 16#007E; %% tilde
code(16#7F) -> 16#007F; %% delete
code(16#80) -> 16#0402; %% cyrillic capital letter dje
code(16#81) -> 16#0403; %% cyrillic capital letter gje
code(16#82) -> 16#201A; %% single low-9 quotation mark
code(16#83) -> 16#0453; %% cyrillic small letter gje
code(16#84) -> 16#201E; %% double low-9 quotation mark
code(16#85) -> 16#2026; %% horizontal ellipsis
code(16#86) -> 16#2020; %% dagger
code(16#87) -> 16#2021; %% double dagger
code(16#88) -> 16#20AC; %% euro sign
code(16#89) -> 16#2030; %% per mille sign
code(16#8A) -> 16#0409; %% cyrillic capital letter lje
code(16#8B) -> 16#2039; %% single left-pointing angle quotation mark
code(16#8C) -> 16#040A; %% cyrillic capital letter nje
code(16#8D) -> 16#040C; %% cyrillic capital letter kje
code(16#8E) -> 16#040B; %% cyrillic capital letter tshe
code(16#8F) -> 16#040F; %% cyrillic capital letter dzhe
code(16#90) -> 16#0452; %% cyrillic small letter dje
code(16#91) -> 16#2018; %% left single quotation mark
code(16#92) -> 16#2019; %% right single quotation mark
code(16#93) -> 16#201C; %% left double quotation mark
code(16#94) -> 16#201D; %% right double quotation mark
code(16#95) -> 16#2022; %% bullet
code(16#96) -> 16#2013; %% en dash
code(16#97) -> 16#2014; %% em dash
code(16#99) -> 16#2122; %% trade mark sign
code(16#9A) -> 16#0459; %% cyrillic small letter lje
code(16#9B) -> 16#203A; %% single right-pointing angle quotation mark
code(16#9C) -> 16#045A; %% cyrillic small letter nje
code(16#9D) -> 16#045C; %% cyrillic small letter kje
code(16#9E) -> 16#045B; %% cyrillic small letter tshe
code(16#9F) -> 16#045F; %% cyrillic small letter dzhe
code(16#A0) -> 16#00A0; %% no-break space
code(16#A1) -> 16#040E; %% cyrillic capital letter short u
code(16#A2) -> 16#045E; %% cyrillic small letter short u
code(16#A3) -> 16#0408; %% cyrillic capital letter je
code(16#A4) -> 16#00A4; %% currency sign
code(16#A5) -> 16#0490; %% cyrillic capital letter ghe with upturn
code(16#A6) -> 16#00A6; %% broken bar
code(16#A7) -> 16#00A7; %% section sign
code(16#A8) -> 16#0401; %% cyrillic capital letter io
code(16#A9) -> 16#00A9; %% copyright sign
code(16#AA) -> 16#0404; %% cyrillic capital letter ukrainian ie
code(16#AB) -> 16#00AB; %% left-pointing double angle quotation mark
code(16#AC) -> 16#00AC; %% not sign
code(16#AD) -> 16#00AD; %% soft hyphen
code(16#AE) -> 16#00AE; %% registered sign
code(16#AF) -> 16#0407; %% cyrillic capital letter yi
code(16#B0) -> 16#00B0; %% degree sign
code(16#B1) -> 16#00B1; %% plus-minus sign
code(16#B2) -> 16#0406; %% cyrillic capital letter byelorussian-ukrainian i
code(16#B3) -> 16#0456; %% cyrillic small letter byelorussian-ukrainian i
code(16#B4) -> 16#0491; %% cyrillic small letter ghe with upturn
code(16#B5) -> 16#00B5; %% micro sign
code(16#B6) -> 16#00B6; %% pilcrow sign
code(16#B7) -> 16#00B7; %% middle dot
code(16#B8) -> 16#0451; %% cyrillic small letter io
code(16#B9) -> 16#2116; %% numero sign
code(16#BA) -> 16#0454; %% cyrillic small letter ukrainian ie
code(16#BB) -> 16#00BB; %% right-pointing double angle quotation mark
code(16#BC) -> 16#0458; %% cyrillic small letter je
code(16#BD) -> 16#0405; %% cyrillic capital letter dze
code(16#BE) -> 16#0455; %% cyrillic small letter dze
code(16#BF) -> 16#0457; %% cyrillic small letter yi
code(16#C0) -> 16#0410; %% cyrillic capital letter a
code(16#C1) -> 16#0411; %% cyrillic capital letter be
code(16#C2) -> 16#0412; %% cyrillic capital letter ve
code(16#C3) -> 16#0413; %% cyrillic capital letter ghe
code(16#C4) -> 16#0414; %% cyrillic capital letter de
code(16#C5) -> 16#0415; %% cyrillic capital letter ie
code(16#C6) -> 16#0416; %% cyrillic capital letter zhe
code(16#C7) -> 16#0417; %% cyrillic capital letter ze
code(16#C8) -> 16#0418; %% cyrillic capital letter i
code(16#C9) -> 16#0419; %% cyrillic capital letter short i
code(16#CA) -> 16#041A; %% cyrillic capital letter ka
code(16#CB) -> 16#041B; %% cyrillic capital letter el
code(16#CC) -> 16#041C; %% cyrillic capital letter em
code(16#CD) -> 16#041D; %% cyrillic capital letter en
code(16#CE) -> 16#041E; %% cyrillic capital letter o
code(16#CF) -> 16#041F; %% cyrillic capital letter pe
code(16#D0) -> 16#0420; %% cyrillic capital letter er
code(16#D1) -> 16#0421; %% cyrillic capital letter es
code(16#D2) -> 16#0422; %% cyrillic capital letter te
code(16#D3) -> 16#0423; %% cyrillic capital letter u
code(16#D4) -> 16#0424; %% cyrillic capital letter ef
code(16#D5) -> 16#0425; %% cyrillic capital letter ha
code(16#D6) -> 16#0426; %% cyrillic capital letter tse
code(16#D7) -> 16#0427; %% cyrillic capital letter che
code(16#D8) -> 16#0428; %% cyrillic capital letter sha
code(16#D9) -> 16#0429; %% cyrillic capital letter shcha
code(16#DA) -> 16#042A; %% cyrillic capital letter hard sign
code(16#DB) -> 16#042B; %% cyrillic capital letter yeru
code(16#DC) -> 16#042C; %% cyrillic capital letter soft sign
code(16#DD) -> 16#042D; %% cyrillic capital letter e
code(16#DE) -> 16#042E; %% cyrillic capital letter yu
code(16#DF) -> 16#042F; %% cyrillic capital letter ya
code(16#E0) -> 16#0430; %% cyrillic small letter a
code(16#E1) -> 16#0431; %% cyrillic small letter be
code(16#E2) -> 16#0432; %% cyrillic small letter ve
code(16#E3) -> 16#0433; %% cyrillic small letter ghe
code(16#E4) -> 16#0434; %% cyrillic small letter de
code(16#E5) -> 16#0435; %% cyrillic small letter ie
code(16#E6) -> 16#0436; %% cyrillic small letter zhe
code(16#E7) -> 16#0437; %% cyrillic small letter ze
code(16#E8) -> 16#0438; %% cyrillic small letter i
code(16#E9) -> 16#0439; %% cyrillic small letter short i
code(16#EA) -> 16#043A; %% cyrillic small letter ka
code(16#EB) -> 16#043B; %% cyrillic small letter el
code(16#EC) -> 16#043C; %% cyrillic small letter em
code(16#ED) -> 16#043D; %% cyrillic small letter en
code(16#EE) -> 16#043E; %% cyrillic small letter o
code(16#EF) -> 16#043F; %% cyrillic small letter pe
code(16#F0) -> 16#0440; %% cyrillic small letter er
code(16#F1) -> 16#0441; %% cyrillic small letter es
code(16#F2) -> 16#0442; %% cyrillic small letter te
code(16#F3) -> 16#0443; %% cyrillic small letter u
code(16#F4) -> 16#0444; %% cyrillic small letter ef
code(16#F5) -> 16#0445; %% cyrillic small letter ha
code(16#F6) -> 16#0446; %% cyrillic small letter tse
code(16#F7) -> 16#0447; %% cyrillic small letter che
code(16#F8) -> 16#0448; %% cyrillic small letter sha
code(16#F9) -> 16#0449; %% cyrillic small letter shcha
code(16#FA) -> 16#044A; %% cyrillic small letter hard sign
code(16#FB) -> 16#044B; %% cyrillic small letter yeru
code(16#FC) -> 16#044C; %% cyrillic small letter soft sign
code(16#FD) -> 16#044D; %% cyrillic small letter e
code(16#FE) -> 16#044E; %% cyrillic small letter yu
code(16#FF) -> 16#044F; %% cyrillic small letter ya
code(Other) -> Other.

%%
%% Test
%%

-ifdef(TEST).
-include("test.hrl").

decode_test() ->
    ?assertEqual(<<>>, win1251:decode("")),
    ?assertEqual(<<"Тест"/utf8>>, win1251:decode("Тест")).

-endif.
