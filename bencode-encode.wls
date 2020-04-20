BencodeEncode[number_Integer, encoding_String:"UTF-8"] :=
    Join@@ToCharacterCode/@{"i", ToString@number, "e"};

BencodeEncode[string_String, encoding_String:"UTF-8"] := 
    BencodeEncode[ByteArray@ToCharacterCode[string, encoding], encoding]

BencodeEncode[bytes_ByteArray, encoding_String:"UTF-8"] := Block[
    {len, str},
    str = Normal@bytes;
    len = ToCharacterCode[ToString@Length@str <> ":"];
    Join[len, str]
]

BencodeEncode[list_List, encoding_String:"UTF-8"] := Block[
    {encList},
    encList = BencodeEncode[#, encoding]& /@ list;
    Flatten@{ToCharacterCode@"l", encList, ToCharacterCode@"e"}
]

BencodeEncode[dict_Association, encoding_String:"UTF-8"] := Block[{encoded},
    encoded = KeyMap[BencodeEncode[#, encoding]&, dict];
    encoded = Map[BencodeEncode[#, encoding]&, encoded];
    encoded = KeyValueMap[Join, encoded];
    Flatten@{ToCharacterCode@"d", encoded, ToCharacterCode@"e"}
];
