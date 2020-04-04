BencodeEncode[number_Integer] :=
    "i" <> ToString@number <> "e";

BencodeEncode[string_String] :=
    ToString@StringLength@string <> ":" <> string;

BencodeEncode[list_List] :=
    "l" <> StringJoin@@BencodeEncode/@list <> "e";

BencodeEncode[dict_Association] := Module[{encoded},
    encoded = Map[BencodeEncode, KeyMap[BencodeEncode, KeySort@dict]];
    "d" <> StringJoin@KeyValueMap[ #1 <> #2 &, encoded] <> "e"
];
