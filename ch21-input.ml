(* vim: set nowrap: -*- truncate-lines: t -*- *)

let input = [[45;6;6;95;70;46;37;97;82;69;44;66;19;5;41;57;53;80;14;32];
             [39;1;96;59;65;73;33;22;33;78;85;63;89;38;83;35;1;55;12;45];
             [43;72;27;58;34;25;20;57;41;22;2;45;17;54;51;26;57;76;55;65];
             [31;42;53;6;73;14;3;91;95;63;57;34;67;46;47;94;17;16;11;7];
             [99;73;42;54;92;48;86;3;99;57;76;70;83;38;6;28;53;13;96;54];
             [6;49;20;42;76;60;18;63;57;83;23;0;60;80;72;2;86;10;4;68];
             [37;89;28;76;18;40;55;63;36;72;23;86;96;52;25;0;82;6;17;64];
             [18;99;88;70;23;87;34;81;36;48;81;80;33;34;62;15;86;37;85;60];
             [88;36;56;42;24;88;28;64;45;92;77;43;12;78;16;34;24;22;8;35];
             [14;33;99;7;7;81;10;44;52;99;19;13;15;22;26;99;5;42;88;45];
             [64;73;21;17;93;19;13;69;7;44;38;23;35;78;23;0;33;52;2;59];
             [92;48;36;24;57;19;24;65;39;67;82;48;4;12;87;94;25;59;18;9];
             [10;18;33;86;78;56;54;48;44;73;47;18;51;58;79;40;97;10;60;30];
             [99;51;89;38;31;21;28;9;61;5;91;75;17;59;46;80;15;2;16;31];
             [4;65;50;88;14;9;77;87;10;15;32;58;98;96;56;11;36;58;38;67];
             [52;73;72;74;73;92;67;93;54;93;24;28;56;94;22;10;42;18;35;97];
             [26;40;72;2;89;72;22;9;56;56;79;50;76;61;5;59;58;50;67;26];
             [27;65;31;59;96;86;26;32;46;37;46;92;87;29;71;18;63;17;80;11];
             [28;94;39;69;73;40;92;5;78;61;52;3;94;77;40;87;59;53;36;20];
             [28;20;93;40;83;0;23;35;38;1;9;47;42;4;57;46;67;56;47;8];
             [92;67;3;12;25;72;76;51;19;27;91;49;74;18;78;85;43;90;34;12];
             [56;42;20;63;71;80;63;16;8;2;36;90;45;60;70;23;24;73;5;69];
             [36;94;71;61;54;88;12;48;97;78;47;0;49;1;74;57;25;49;19;76];
             [58;29;9;93;36;8;83;55;86;48;22;55;82;99;46;77;68;71;83;42];
             [55;29;72;56;51;79;47;84;12;98;33;93;81;79;23;78;97;59;47;83];
             [77;62;19;81;88;36;42;48;56;66;22;14;18;57;85;75;62;64;24;1];
             [21;53;90;67;6;21;40;91;5;55;19;93;15;44;20;31;0;54;37;59];
             [49;90;15;0;61;63;56;93;30;9;36;41;94;92;28;86;32;91;51;40];
             [56;46;16;29;86;32;25;80;18;22;40;92;57;47;30;64;53;98;40;44];
             [49;91;78;42;45;36;73;71;42;61;91;35;6;9;80;89;39;69;92;51];
             [52;89;49;32;98;15;75;14;20;78;24;49;17;65;69;28;56;12;84;63];
             [26;70;41;61;0;46;8;58;36;76;94;32;46;18;24;95;20;79;52;44];
             [32;72;98;50;88;77;54;0;4;25;60;86;13;43;49;60;8;12;59;87];
             [39;73;69;68;8;14;67;10;75;23;28;47;72;76;44;33;27;33;52;23];
             [64;85;54;55;25;18;88;66;45;81;31;49;6;22;47;52;49;32;33;6];
             [80;30;36;18;35;11;90;78;87;96;3;25;63;14;48;40;76;39;79;61];
             [51;42;43;71;5;4;52;34;6;37;86;60;81;98;55;28;22;82;95;81];
             [33;32;65;30;18;37;50;92;12;44;66;7;91;83;6;29;69;25;78;80];
             [49;94;7;67;72;28;11;65;28;48;29;38;11;4;2;34;89;74;55;42];
             [31;35;54;96;69;21;7;39;33;61;86;47;77;12;38;3;44;66;54;39];
             [54;57;35;31;90;30;59;99;59;37;76;89;70;98;31;88;70;11;71;49];
             [21;20;35;37;3;0;27;8;57;58;87;29;99;63;54;87;43;58;93;42];
             [31;12;91;69;55;30;98;99;76;69;23;75;25;11;92;5;56;48;32;92];
             [36;24;49;11;49;43;40;65;36;34;87;14;4;25;10;66;70;80;74;42];
             [12;99;18;55;98;23;50;73;47;71;75;80;23;12;26;2;73;97;92;99];
             [35;72;48;30;12;1;74;58;59;2;89;14;75;16;78;29;59;87;59;94];
             [0;28;28;11;83;88;91;47;29;25;72;99;15;46;90;50;72;79;93;6];
             [70;78;86;89;93;47;40;35;95;39;76;28;39;80;64;24;29;46;43;70];
             [15;43;11;33;43;50;65;68;72;1;84;67;34;95;95;79;36;59;81;47];
             [1;56;45;75;68;69;76;64;28;74;17;32;5;44;46;0;28;10;87;1];
             [83;0;96;74;29;49;87;67;21;3;5;91;63;42;0;36;38;24;96;63];
             [73;5;92;61;22;22;43;32;13;39;51;52;95;6;65;59;47;98;62;81];
             [81;44;57;20;87;44;61;27;79;52;30;31;4;23;96;37;18;50;14;98];
             [90;64;96;29;37;32;42;52;23;76;33;38;18;34;55;4;68;47;68;0];
             [55;69;70;74;20;66;52;88;88;60;75;11;6;20;77;93;43;16;36;35];
             [31;70;22;12;66;97;88;54;6;8;0;50;24;72;3;7;75;72;83;58];
             [30;29;25;50;39;78;11;81;79;78;16;15;82;17;58;65;3;98;90;45];
             [39;51;12;45;8;65;87;39;43;27;30;32;89;49;44;75;13;67;46;30];
             [44;94;42;63;91;50;34;83;28;82;34;54;62;40;76;24;66;93;65;12];
             [56;99;17;84;74;41;87;81;40;3;48;47;56;81;1;22;11;22;46;63];
             [20;65;81;95;56;62;46;68;44;54;65;38;83;49;94;73;41;4;8;67];
             [18;29;22;28;87;23;29;95;45;77;82;78;0;97;42;48;88;48;22;25];
             [45;69;39;99;35;71;56;17;10;86;81;42;53;73;88;69;42;70;68;18];
             [80;22;14;50;61;26;99;51;4;71;64;55;46;55;65;11;95;13;17;2];
             [27;39;51;13;83;50;63;10;62;99;64;73;42;79;69;69;11;5;60;42];
             [86;7;88;73;24;84;2;37;56;16;97;72;77;30;51;39;4;32;46;30];
             [81;93;91;67;87;80;74;79;73;78;47;96;31;9;57;80;15;50;98;27];
             [29;19;40;88;15;35;40;48;76;50;67;4;76;14;31;45;35;64;76;42];
             [56;14;72;93;0;38;74;81;5;75;10;63;94;53;78;45;42;89;83;49];
             [44;94;46;8;64;40;96;39;43;16;74;99;75;81;10;80;26;12;42;25];
             [81;77;22;56;24;19;79;73;34;3;11;45;27;86;88;41;19;43;85;31];
             [78;67;90;16;51;52;58;91;1;22;81;71;4;55;86;62;83;95;0;67];
             [59;4;22;31;80;23;84;50;19;53;43;39;46;63;97;76;11;59;38;76];
             [11;37;84;83;0;6;23;80;65;15;36;23;65;26;2;13;41;60;43;69];
             [96;63;65;70;94;61;13;94;6;21;57;48;29;97;37;67;2;13;45;28];
             [71;52;14;63;65;84;15;0;81;82;80;40;35;1;47;24;59;56;39;54];
             [79;19;69;8;12;37;69;24;44;65;35;70;96;63;53;1;73;53;1;80];
             [53;70;15;88;22;49;59;99;35;66;74;75;48;19;80;88;23;20;89;62];
             [54;81;96;47;99;77;28;4;58;1;81;47;79;53;82;13;17;32;45;44];
             [53;10;92;0;40;49;59;49;52;95;9;49;6;56;23;2;90;11;17;33];
             [9;82;10;59;23;5;82;3;61;75;97;24;47;16;39;22;71;88;48;61];
             [86;46;81;41;55;4;88;46;51;20;58;97;97;82;99;71;63;72;16;54];
             [18;82;5;59;3;36;74;24;92;20;5;27;75;33;28;76;71;14;79;11];
             [0;45;17;31;54;35;39;7;89;73;70;9;22;48;27;62;22;68;20;0];
             [31;33;61;42;47;7;84;26;29;66;94;33;24;38;24;86;64;38;40;21];
             [81;99;90;57;21;92;47;63;61;61;55;44;9;88;81;88;72;37;74;25];
             [17;1;90;86;57;44;84;71;20;21;12;32;41;9;72;76;11;49;30;64];
             [25;31;32;90;11;25;71;26;92;42;96;98;83;14;28;13;9;75;84;30];
             [79;39;42;45;65;24;9;47;99;4;11;43;69;74;66;16;80;49;22;28];
             [10;98;58;54;69;73;32;83;69;26;10;76;87;77;40;59;62;46;48;87];
             [72;87;26;83;80;48;5;79;2;17;79;11;98;20;99;88;7;68;38;78];
             [11;10;64;18;83;82;43;5;74;88;77;80;83;62;34;38;56;96;72;97];
             [99;87;80;40;74;4;93;82;74;68;66;88;21;90;74;19;15;67;42;80];
             [7;40;15;77;97;1;47;66;60;84;91;79;26;87;18;1;43;24;44;77];
             [46;1;89;36;57;25;74;34;8;24;61;62;3;74;9;77;18;65;90;85];
             [93;4;83;20;91;82;12;18;17;51;76;49;37;50;45;95;27;96;81;66];
             [4;20;10;22;99;22;54;46;17;62;67;34;9;1;39;21;12;9;65;45];
             [72;13;96;14;85;25;74;26;68;12;28;64;69;4;90;33;66;5;2;52];
             [48;99;1;5;23;78;50;33;91;54;85;34;97;35;93;2;96;87;28;31];
             [29;11;5;6;27;57;19;62;72;40;62;38;45;93;73;26;44;74;55;37];
             [89;2;22;40;56;9;32;91;18;80;92;47;78;60;88;79;23;34;89;43];
             [80;66;83;2;84;39;75;61;14;34;51;27;70;65;6;25;85;66;49;58];
             [30;43;92;7;96;54;72;62;72;9;66;17;7;67;54;34;64;23;13;98];
             [48;62;38;84;67;79;45;47;91;30;67;30;98;14;63;94;27;96;51;3];
             [47;38;11;94;67;77;35;98;76;79;17;69;26;55;17;34;36;86;60;87];
             [61;68;88;28;31;89;10;2;19;7;50;39;86;65;69;73;76;38;13;85];
             [5;91;79;40;49;99;7;75;22;36;9;46;9;94;31;17;57;80;58;47];
             [83;72;64;9;99;95;2;95;66;52;47;85;6;6;67;86;88;19;26;87];
             [71;59;88;76;6;79;6;18;34;37;59;98;62;25;43;23;67;24;10;93];
             [81;11;49;17;18;43;16;59;98;13;59;46;73;51;61;92;35;80;76;2];
             [8;92;14;92;29;98;12;93;15;48;48;75;53;3;78;8;74;1;53;72];
             [20;16;87;87;52;51;61;39;94;84;23;95;29;2;62;38;20;79;29;57];
             [27;3;1;45;66;11;73;14;79;97;89;89;83;6;24;69;89;58;2;51];
             [50;84;83;43;22;96;27;94;91;52;94;24;78;89;5;9;34;47;82;17];
             [83;62;66;61;91;97;88;80;72;22;26;37;29;43;28;69;33;71;47;26];
             [71;74;5;53;35;87;73;71;7;48;32;87;35;76;32;54;60;91;24;3];
             [25;57;27;3;75;20;98;35;57;43;66;35;93;64;65;90;73;91;88;80];
             [97;26;80;61;56;37;45;41;25;99;1;29;34;73;34;97;56;4;21;50];
             [45;83;88;8;87;44;25;87;46;51;40;55;49;9;7;27;39;70;96;15];
             [42;77;3;53;13;56;20;64;46;4;13;96;93;56;54;25;73;9;80;9];
             [96;89;28;92;25;53;21;30;49;16;71;44;40;10;35;74;32;58;76;16];
             [21;3;21;84;2;39;32;81;56;58;12;21;62;53;29;26;37;45;10;42];
             [62;88;49;19;46;59;96;87;39;88;32;1;18;54;30;14;44;27;63;51];
             [90;4;41;92;70;42;70;73;56;70;40;62;66;41;10;20;11;65;85;48];
             [43;97;47;69;46;9;5;91;87;25;11;10;28;61;2;75;95;95;37;4];
             [78;92;57;9;18;36;8;3;13;34;67;52;62;69;65;56;40;66;85;84];
             [24;53;48;11;36;62;38;12;80;84;33;47;64;85;37;65;5;7;9;98];
             [62;74;19;28;88;1;3;52;10;0;45;99;32;96;20;73;61;40;64;30];
             [62;5;98;58;45;8;37;30;84;52;40;2;21;4;61;81;45;76;52;27];
             [5;48;62;60;53;92;39;94;37;12;15;25;44;97;93;7;91;78;52;28];
             [47;60;25;86;48;78;97;59;74;2;22;78;80;14;94;79;1;29;54;83];
             [10;47;26;42;41;33;83;1;48;26;35;43;9;5;89;65;45;12;70;98];
             [61;26;10;60;55;30;29;92;37;29;88;64;28;26;76;47;71;81;34;46];
             [62;56;40;24;32;72;75;8;36;31;60;65;70;21;44;8;25;6;37;67];
             [60;65;65;65;43;64;5;25;86;58;73;99;36;1;29;65;66;7;83;4];
             [59;7;62;57;94;37;2;30;39;11;75;38;57;19;25;76;67;11;80;66];
             [56;39;42;49;50;47;23;31;24;45;43;47;82;20;22;37;51;85;65;9];
             [5;42;63;3;79;89;18;39;22;50;57;37;64;50;62;86;47;23;31;3];
             [78;63;75;80;18;6;51;73;70;81;97;7;15;3;87;79;10;43;31;18];
             [16;63;11;2;18;81;89;77;12;11;27;99;56;90;85;58;24;16;25;34];
             [11;48;11;51;72;78;19;31;75;87;67;79;33;58;62;31;9;65;67;9];
             [51;60;3;44;70;86;88;17;50;26;11;32;44;54;70;36;88;96;48;32];
             [20;5;59;62;15;48;4;56;56;81;11;88;87;69;3;49;25;87;31;55];
             [48;49;22;19;54;11;32;55;11;69;34;43;41;99;42;40;35;48;63;8];
             [44;32;25;50;57;89;43;97;4;36;83;2;13;37;67;55;78;77;7;24];
             [88;74;37;42;36;28;7;96;85;18;51;83;30;44;59;53;33;42;31;86];
             [4;90;30;54;69;0;92;70;55;3;92;83;10;74;22;83;44;84;96;84];
             [19;18;97;60;58;23;65;10;75;26;76;2;74;35;15;84;36;31;12;66];
             [7;17;82;39;96;73;11;13;21;55;98;94;24;75;92;85;68;23;48;53];
             [21;5;17;62;17;53;46;75;52;14;32;35;80;59;82;62;36;36;5;11];
             [44;21;83;35;38;81;2;7;48;99;18;6;87;75;63;46;16;38;0;85];
             [81;51;91;57;21;1;5;4;55;60;78;98;26;78;39;7;96;23;40;99];
             [89;45;69;32;41;36;90;69;74;57;64;1;71;31;98;6;23;71;59;27];
             [96;16;66;27;51;97;56;53;2;47;40;5;65;77;74;60;0;2;48;98];
             [94;97;88;82;10;22;62;33;67;6;24;34;74;38;7;95;50;69;96;51];
             [19;33;20;35;48;36;91;98;26;98;69;76;40;28;89;64;62;23;12;28];
             [36;56;56;75;49;49;48;31;68;43;22;29;48;82;57;9;83;87;40;97];
             [67;48;57;51;96;52;64;16;28;30;4;25;89;64;8;12;13;20;68;49];
             [79;57;24;71;61;41;1;18;46;67;80;75;18;43;21;42;60;57;68;6];
             [82;95;8;53;46;27;62;84;40;98;23;2;25;86;47;26;68;70;95;84];
             [46;58;63;71;29;81;85;98;6;56;80;29;55;17;28;55;82;26;73;74];
             [85;90;94;44;30;86;32;39;68;86;69;34;76;28;29;51;69;93;69;48];
             [15;16;27;74;74;28;66;53;58;79;15;12;79;71;13;40;52;85;83;62];
             [45;95;52;31;71;37;39;44;87;21;13;86;89;80;22;2;73;84;83;80];
             [16;47;96;88;10;21;78;20;85;8;86;48;66;14;27;7;76;7;66;35];
             [20;66;98;37;87;26;80;50;43;55;61;8;55;57;23;68;41;19;42;46];
             [9;25;37;52;6;96;52;64;58;12;37;29;29;27;48;8;76;43;73;20];
             [50;43;96;94;96;78;91;3;74;77;28;8;50;49;5;88;32;65;51;71];
             [0;60;38;87;22;8;40;13;87;8;83;61;72;34;21;23;83;91;75;67];
             [87;72;96;80;52;87;1;60;86;46;0;15;65;5;54;86;30;45;32;5];
             [79;18;66;45;30;68;46;47;23;89;24;92;51;47;54;31;27;83;1;92];
             [97;58;33;15;20;20;11;29;17;95;38;24;88;48;64;15;86;76;75;86];
             [42;20;86;7;19;30;28;68;99;43;29;27;49;9;17;14;42;18;31;93];
             [48;44;4;70;24;42;49;92;43;71;26;56;58;64;96;49;94;80;19;84];
             [64;43;94;17;87;43;56;0;45;17;30;8;44;66;65;23;75;24;53;46];
             [44;85;37;76;0;59;1;80;63;64;54;46;22;86;66;3;53;51;47;63];
             [69;69;33;82;57;31;4;57;25;69;33;97;49;49;77;44;75;80;88;42];
             [60;49;0;84;0;3;80;13;94;90;24;17;83;13;95;11;91;77;84;81];
             [27;79;57;28;7;61;44;73;94;95;6;5;67;88;9;46;93;13;22;91];
             [77;51;98;39;78;55;39;34;93;22;57;56;89;95;85;29;75;38;93;95];
             [99;83;57;22;19;25;92;25;60;22;10;20;14;91;68;64;6;3;68;3];
             [58;43;64;70;95;43;91;52;11;89;26;35;44;60;92;31;65;26;33;69];
             [87;69;21;62;14;29;97;45;76;28;25;30;14;29;72;75;40;47;16;99];
             [13;57;82;21;84;2;4;30;69;52;49;93;93;56;69;32;63;43;30;85];
             [48;3;66;1;62;55;22;8;35;8;75;96;5;34;45;28;38;95;42;22];
             [81;61;5;7;19;60;70;95;67;41;79;68;63;12;58;77;87;64;26;98];
             [11;96;33;63;55;87;80;55;51;3;79;14;32;95;54;95;94;95;13;46];
             [8;5;14;0;81;90;35;24;77;76;76;58;14;79;13;1;69;62;31;83];
             [3;6;10;38;71;23;38;72;53;37;64;21;89;51;73;70;43;22;10;40];
             [89;68;45;18;52;51;46;71;11;36;62;40;49;72;42;81;20;18;11;51];
             [56;51;73;87;97;91;27;63;29;53;22;40;2;62;92;23;92;18;18;81];
             [5;0;49;9;6;92;94;74;41;21;9;32;73;62;40;33;77;6;83;54];
             [7;45;14;77;41;58;79;88;20;1;6;52;58;38;46;51;51;28;37;88];
             [47;60;85;41;28;24;0;13;16;70;52;66;18;90;71;82;95;37;10;0];
             [56;12;91;87;3;21;38;31;89;44;92;67;15;76;47;11;78;92;69;86];
             [71;52;82;31;93;48;0;3;77;5;97;35;53;85;29;17;87;68;74;29];
             [42;20;68;28;97;23;84;23;57;62;17;96;75;80;36;50;47;99;50;23];
             [16;88;81;22;9;41;14;81;71;9;50;97;85;49;74;89;87;0;53;23];
             [78;31;69;96;96;36;76;21;74;26;50;10;98;67;21;55;22;31;87;8];
             [52;53;50;26;69;79;67;10;57;41;54;30;95;15;51;22;78;57;59;78];
             [81;33;81;16;24;13;0;86;56;96;77;33;8;6;86;39;9;89;22;10];
             [44;82;36;19;88;63;87;36;63;21;97;89;6;82;90;79;66;80;65;75];
             [71;93;2;98;9;91;3;28;25;14;8;47;50;37;43;18;81;80;62;20];
             [51;83;0;46;50;7;36;41;9;41;54;20;86;86;48;27;56;84;82;48];
             [86;68;20;85;22;47;29;79;51;62;27;75;47;9;95;91;86;21;57;22];
             [3;57;86;8;9;94;54;71;82;32;80;0;33;65;69;34;62;40;39;89];
             [48;16;28;75;29;62;75;68;10;74;83;22;1;47;74;92;21;67;74;84];
             [34;91;23;3;14;12;62;43;56;75;23;22;61;64;55;76;49;3;29;61];
             [54;38;4;97;5;74;26;9;9;33;77;61;21;71;20;10;99;87;69;74];
             [99;75;27;61;71;63;15;71;95;1;1;66;70;38;23;34;52;77;81;3];
             [40;66;98;27;88;63;44;87;24;38;68;98;15;88;10;3;23;52;8;4];
             [28;18;31;44;57;2;77;32;22;17;28;11;78;42;1;26;48;87;78;26];
             [51;28;22;27;72;3;5;74;98;43;90;24;87;13;15;10;79;87;43;16];
             [53;11;15;53;97;95;16;65;13;35;49;37;37;31;47;36;10;10;6;43];
             [79;7;67;57;75;71;88;3;18;33;81;87;14;11;25;47;74;29;14;53];
             [56;25;37;61;97;82;92;40;6;45;73;47;23;95;65;16;67;46;31;35];
             [65;80;71;98;85;39;22;64;38;27;35;34;44;87;16;92;35;33;84;90];
             [98;2;68;16;69;60;29;42;29;35;83;2;22;22;8;65;81;1;29;19];
             [59;28;21;51;63;28;17;0;82;67;11;79;39;18;11;8;38;92;27;61];
             [33;72;49;64;90;95;83;97;1;5;91;32;31;33;39;82;4;26;78;16];
             [73;37;91;16;53;73;86;11;5;14;21;93;68;55;86;46;85;64;3;82];
             [22;61;84;93;15;25;26;32;51;28;97;6;77;49;30;99;64;92;41;54];
             [7;60;34;26;19;31;87;1;22;19;54;0;9;93;95;1;16;91;96;37];
             [94;39;20;69;34;75;50;51;31;49;91;78;28;98;79;30;83;68;61;50];
             [8;12;28;17;15;95;38;32;90;64;13;13;14;15;78;61;99;10;62;41];
             [12;62;49;31;74;91;76;32;40;36;81;0;96;61;73;47;6;99;7;24];
             [32;7;63;61;57;33;50;14;1;97;65;5;60;72;86;53;31;15;47;19];
             [95;27;37;27;18;46;96;9;87;85;96;66;16;33;8;15;44;40;58;67];
             [74;74;83;25;89;66;64;91;63;8;94;74;0;94;99;92;24;8;10;55];
             [62;78;61;54;59;24;52;45;22;31;79;67;89;29;42;18;25;56;18;36];
             [16;2;22;71;85;27;93;45;32;9;59;83;60;89;21;39;65;97;92;92];
             [3;11;35;80;95;5;87;26;95;3;9;43;43;6;44;1;17;3;1;97];
             [36;10;44;82;85;39;92;65;95;92;87;10;37;95;86;36;29;30;25;29];
             [32;45;69;37;91;35;72;24;31;58;60;58;26;56;68;59;96;97;12;73];
             [7;46;10;91;13;22;51;5;1;55;12;60;62;65;42;8;26;75;75;75];
             [5;76;17;13;24;66;4;82;93;1;99;8;21;67;75;4;88;18;13;71];
             [45;33;90;79;24;65;11;35;28;84;22;70;79;76;58;70;9;43;88;54];
             [30;5;58;0;54;17;25;20;2;38;7;11;73;58;78;58;47;6;85;54];
             [59;30;65;66;36;32;93;41;25;88;49;32;83;80;73;34;21;63;67;11];
             [6;12;40;44;8;91;55;8;7;4;9;75;24;11;46;22;34;21;45;40];
             [81;67;49;42;8;11;5;73;20;76;25;17;28;7;70;14;81;86;20;60];
             [88;20;38;69;58;0;72;83;92;27;88;83;4;22;21;22;8;84;28;16];
             [49;95;41;7;43;53;91;86;42;61;9;40;72;44;60;47;90;70;88;33];
             [49;13;82;41;58;83;47;52;12;12;29;7;15;52;76;63;2;31;35;29];
             [17;75;5;64;86;45;59;68;46;38;15;38;99;29;1;90;15;92;80;58];
             [40;68;14;87;96;95;10;28;10;32;84;23;13;15;32;64;33;7;26;59];
             [61;52;21;76;4;71;61;95;27;23;19;28;22;12;25;16;38;26;93;79];
             [74;77;24;93;89;36;38;23;54;92;83;39;26;57;49;3;18;88;30;48];
             [49;13;40;34;69;57;83;89;2;10;66;80;53;56;74;90;79;84;76;11];
             [90;64;6;51;11;61;47;94;99;9;52;99;79;60;45;74;80;89;37;8];
             [23;19;43;74;23;99;71;23;9;46;17;23;19;99;31;94;15;6;72;46];
             [47;12;70;21;18;22;40;17;21;58;56;34;42;57;89;45;61;90;24;41];
             [56;13;32;96;38;70;61;85;16;75;41;68;22;93;98;88;15;84;13;86];
             [57;84;59;2;38;77;10;54;9;68;79;57;39;37;67;99;85;29;0;51];
             [24;50;58;20;3;98;31;44;21;64;66;51;51;75;90;78;14;67;45;45];
             [48;42;22;81;77;18;15;10;99;33;44;42;97;82;35;41;56;29;70;96];
             [19;41;53;48;54;82;73;26;2;42;3;72;32;64;95;19;72;46;58;74];
             [59;53;96;29;36;61;87;2;15;12;95;9;84;29;29;18;49;12;92;30];
             [83;96;77;19;46;39;84;71;92;14;85;53;89;80;91;6;53;58;59;54];
             [61;71;61;14;64;99;12;55;89;59;99;4;87;36;8;93;86;26;18;65];
             [48;33;36;73;98;9;86;5;38;47;25;68;83;13;21;21;71;19;92;87];
             [8;91;74;77;65;56;48;22;41;4;22;22;88;34;63;0;15;6;5;77];
             [30;74;96;35;7;67;9;16;71;52;65;84;48;47;27;67;12;10;42;76];
             [97;22;89;2;24;77;71;76;86;85;55;68;21;98;68;39;72;56;49;60];
             [45;30;16;51;8;19;55;72;80;36;17;79;89;59;43;25;37;27;57;49];
             [11;61;35;98;43;59;17;14;71;25;50;42;60;39;37;14;1;94;15;7];
             [24;44;4;55;90;58;37;29;15;65;29;94;87;54;66;91;41;31;79;96];
             [2;45;36;20;59;64;35;6;0;90;14;59;70;16;74;76;34;46;4;27];
             [82;45;37;73;87;76;2;55;85;53;16;12;14;19;95;17;71;75;37;67];
             [68;43;92;49;32;4;57;72;5;98;55;16;77;60;39;28;39;5;68;13];
             [39;3;41;40;59;36;83;52;42;45;81;87;46;41;27;98;53;77;9;35];
             [2;71;5;67;49;61;48;63;4;71;64;87;16;57;36;85;16;13;13;22];
             [12;93;66;65;98;98;96;15;96;27;13;62;37;57;29;79;14;36;7;33];
             [67;2;39;25;2;5;76;9;35;57;74;25;35;95;5;25;16;39;58;87];
             [41;58;15;13;72;92;11;68;22;1;77;63;17;17;36;74;79;97;53;48];
             [70;1;90;58;35;84;3;39;39;56;51;22;86;59;36;48;62;99;49;97];
             [13;97;14;38;51;74;74;51;24;86;2;26;93;63;30;1;94;61;98;28];
             [47;19;22;44;12;81;74;65;19;8;96;95;34;76;28;73;66;35;78;84];
             [15;11;28;2;75;28;13;65;30;71;28;75;62;0;61;39;45;15;33;97];
             [97;9;39;81;76;44;34;28;25;22;82;99;7;16;24;61;12;16;45;90];
             [13;68;42;96;89;20;84;24;68;25;65;4;39;65;6;94;9;14;33;60];
             [64;74;36;91;85;27;95;70;74;34;99;60;37;74;15;6;16;72;34;74];
             [10;22;48;32;68;15;72;1;49;65;14;52;52;58;72;46;40;6;1;45];
             [50;34;72;79;63;98;17;26;33;87;0;9;12;34;7;64;5;55;39;46];
             [98;25;17;3;80;24;58;99;81;8;24;98;48;55;68;33;26;61;55;97];
             [80;44;52;2;65;98;36;77;59;60;83;55;36;81;40;11;34;57;22;55];
             [87;75;49;1;27;76;93;66;98;70;59;1;65;48;86;45;84;57;44;85];
             [34;29;2;7;14;75;95;11;27;65;6;61;73;85;63;19;19;45;75;84];
             [69;20;88;1;26;74;53;47;16;23;96;85;62;59;78;21;98;6;31;24];
             [2;86;93;94;4;7;77;84;64;3;54;29;8;45;44;49;44;19;13;85];
             [32;40;55;5;52;81;41;81;21;55;2;83;44;47;64;41;76;60;40;16];
             [84;91;56;76;70;63;22;22;95;30;34;94;57;66;97;2;68;66;77;22];
             [61;51;82;0;53;28;90;80;11;61;9;1;22;51;95;10;17;51;81;35];
             [10;16;62;90;25;24;38;33;46;52;10;77;29;79;96;98;13;65;43;2];
             [16;40;54;61;45;3;21;18;22;92;35;63;20;15;28;97;70;7;1;51];
             [91;51;55;27;83;57;78;64;28;32;89;37;55;55;73;84;38;39;49;98];
             [72;3;4;65;83;28;63;74;58;25;19;2;50;2;73;19;39;33;54;51];
             [93;28;23;20;26;33;49;64;77;88;28;25;84;19;12;75;29;67;43;55];
             [89;36;9;82;45;13;9;96;37;17;91;12;87;20;50;72;70;38;22;93];
             [4;23;15;52;89;56;9;75;32;2;2;57;18;20;39;22;91;76;60;66];
             [46;16;66;65;96;80;43;3;92;16;95;48;90;75;66;7;68;85;27;81];
             [22;17;17;60;50;41;39;25;80;8;51;50;87;21;32;15;47;41;87;37];
             [66;68;35;73;72;79;81;97;55;16;52;42;38;51;85;26;72;38;81;65];
             [88;59;4;89;86;31;48;32;95;47;95;45;70;9;98;52;7;31;20;55];
             [74;59;39;13;54;73;96;90;7;75;2;23;1;91;42;90;1;42;12;59];
             [10;14;64;10;2;32;69;39;8;13;36;78;67;35;27;22;86;8;98;48];
             [33;69;15;55;13;57;44;93;48;26;0;95;56;96;59;30;40;36;58;30];
             [99;41;56;56;46;58;74;34;65;1;6;61;77;75;91;55;70;68;71;68];
             [60;6;8;78;48;24;52;48;60;29;5;16;82;43;8;70;26;89;37;6];
             [79;36;29;74;39;10;4;18;25;59;38;56;52;27;3;15;83;42;96;93];
             [80;52;18;48;91;38;46;26;51;6;70;35;57;53;62;51;48;82;16;19];
             [91;27;7;49;30;48;40;10;34;28;19;31;40;84;6;97;40;30;45;62];
             [15;17;25;8;36;88;66;74;78;94;42;77;6;63;38;13;16;43;50;59];
             [73;48;58;30;95;71;34;78;51;78;60;29;55;61;6;98;69;78;2;6];
             [14;72;39;44;96;64;54;73;93;97;77;18;10;41;27;27;40;50;73;96];
             [51;23;64;40;47;56;93;9;73;69;31;98;66;29;41;20;77;21;34;91];
             [2;13;2;14;61;45;23;26;81;66;14;97;77;31;91;97;27;96;56;98];
             [10;95;36;17;80;59;64;6;17;76;68;30;69;21;94;64;48;17;43;9];
             [64;32;25;68;8;23;18;41;55;24;63;86;93;4;1;71;25;23;98;13];
             [86;47;46;53;10;40;84;75;51;86;93;84;50;5;9;2;85;60;52;79];
             [90;51;99;68;19;24;70;55;46;2;35;80;9;93;42;76;83;98;96;52];
             [72;22;60;60;7;74;45;84;53;42;0;92;51;26;34;24;35;43;24;80];
             [74;34;34;66;46;45;5;16;33;76;64;58;20;75;12;89;34;58;87;49];
             [13;96;23;58;33;67;34;52;42;31;33;14;78;24;72;73;67;22;84;6];
             [76;23;6;43;18;40;5;52;19;18;59;37;35;39;73;67;61;47;77;17];
             [94;99;83;11;13;30;3;34;31;87;75;15;55;19;42;89;9;52;51;59];
             [22;20;85;16;63;71;56;21;79;54;64;82;26;18;33;66;32;68;54;50];
             [92;24;3;86;8;2;59;12;48;67;20;13;0;45;94;13;91;62;6;73];
             [24;41;14;13;48;14;71;16;63;47;75;10;8;33;95;22;91;48;52;78];
             [33;86;13;85;88;14;68;34;59;14;90;77;21;91;70;48;9;87;37;76];
             [97;9;29;73;94;28;89;35;93;33;28;3;59;64;13;90;80;55;92;87];
             [90;98;30;57;78;68;57;32;59;22;24;19;88;56;42;52;30;61;28;18];
             [74;50;67;68;74;9;75;77;28;11;35;75;96;4;80;59;30;32;95;87];
             [41;34;90;49;44;54;26;1;26;50;98;36;99;78;24;26;70;71;34;71];
             [21;62;40;92;5;77;45;73;93;81;98;2;78;42;94;29;87;2;83;28];
             [83;84;36;81;59;10;93;40;90;46;81;29;45;57;57;68;59;8;49;43];
             [85;88;3;31;85;15;40;25;84;44;18;98;99;86;64;9;79;27;43;61];
             [89;53;71;96;5;92;54;21;96;10;99;46;13;10;72;11;99;34;51;14];
             [28;93;32;8;15;66;49;69;29;41;87;48;39;8;57;24;18;68;88;47];
             [46;49;30;42;59;52;89;17;20;1;60;69;4;19;75;89;37;90;5;80];
             [38;65;14;29;67;19;47;74;7;33;90;96;27;85;69;1;17;79;25;12];
             [87;9;6;99;36;49;33;55;14;17;37;23;74;31;23;95;92;32;10;78];
             [22;76;6;33;68;31;71;85;77;97;14;75;66;18;75;17;64;11;54;32];
             [94;72;32;74;79;95;10;25;10;15;93;56;79;59;86;54;12;99;50;31];
             [21;13;87;93;95;90;67;67;93;44;31;61;31;87;33;29;77;43;69;71];
             [86;44;37;73;46;16;15;83;19;50;56;65;21;27;68;85;72;87;93;27];
             [0;31;94;5;84;7;75;26;11;35;52;93;95;17;89;48;97;36;62;64];
             [68;15;89;39;11;66;0;43;39;23;55;52;82;49;53;97;90;9;60;30];
             [29;61;89;15;82;97;6;15;1;95;11;48;85;5;30;44;42;94;4;10];
             [77;14;33;11;2;60;36;62;79;3;59;62;61;22;56;99;92;12;52;60];
             [23;76;42;58;26;81;43;76;68;52;7;80;31;67;89;22;37;77;15;13];
             [17;25;36;67;46;83;20;72;64;77;63;72;88;18;5;69;43;29;31;65];
             [91;83;44;90;65;94;92;0;20;73;49;23;91;66;74;71;65;55;70;58];
             [12;41;78;64;63;79;97;83;40;94;79;6;2;89;75;73;72;81;83;44];
             [16;61;43;71;88;35;32;89;72;19;65;91;85;99;38;61;84;13;96;81];
             [49;88;53;13;10;0;44;51;51;42;69;39;32;90;37;24;65;35;93;98];
             [67;4;36;18;74;95;40;54;84;61;74;21;63;72;36;63;98;26;60;36];
             [45;76;54;47;7;25;38;67;19;2;57;51;0;47;23;14;52;9;12;66];
             [39;57;2;48;69;36;94;50;95;84;93;8;59;62;50;76;80;26;78;68];
             [69;12;24;8;98;63;96;29;25;93;81;81;47;83;52;41;73;47;90;87];
             [69;90;27;62;94;12;86;40;76;72;59;18;54;11;96;5;32;57;62;70];
             [37;77;36;28;16;71;64;90;43;94;81;23;82;80;28;26;52;53;35;18];
             [15;21;11;54;4;89;75;76;84;60;79;7;97;36;19;34;26;38;56;38];
             [43;49;20;99;22;4;59;3;23;42;95;29;63;33;52;26;55;26;8;2];
             [28;70;13;38;82;22;2;31;47;18;25;36;98;77;45;97;69;6;10;67];
             [62;15;67;41;7;23;80;35;38;12;61;62;94;31;70;19;66;46;4;44];
             [9;70;55;90;73;96;25;90;38;46;1;54;84;11;4;10;30;9;13;2];
             [86;89;11;38;81;94;12;23;38;96;24;90;21;19;70;18;71;60;15;72];
             [15;92;22;39;60;44;9;90;70;4;3;68;36;78;66;57;17;40;92;18];
             [39;53;68;71;60;74;55;32;81;94;92;36;37;42;34;60;79;1;36;44];
             [90;24;60;76;79;21;2;40;52;78;5;79;19;79;36;47;34;34;47;85];
             [66;48;12;66;67;8;21;51;69;86;26;76;34;75;34;6;51;98;58;46];
             [22;44;58;36;62;86;11;60;50;0;0;20;69;21;92;36;65;69;29;47];
             [10;49;56;55;84;99;80;86;89;24;78;43;93;57;58;3;43;19;59;1];
             [44;71;0;29;54;46;30;57;68;34;37;49;84;0;55;30;8;0;5;15];
             [73;89;33;92;65;30;94;10;19;2;94;76;52;32;31;72;14;64;4;67];
             [46;24;86;66;51;98;6;7;20;32;90;90;4;19;35;13;51;24;97;42];
             [11;6;96;67;72;75;71;8;47;83;61;88;83;33;71;92;47;5;55;92];
             [81;82;45;4;1;81;17;5;67;0;4;79;77;95;28;74;66;17;21;3];
             [96;31;86;52;8;30;71;24;67;22;53;64;65;21;7;6;25;18;94;65];
             [79;69;80;91;68;30;8;47;65;19;43;14;10;70;5;89;49;52;48;63];
             [64;97;77;1;68;23;74;37;61;31;30;28;33;5;5;16;70;21;79;84];
             [12;30;52;33;11;90;13;97;31;10;6;30;34;78;55;54;69;80;22;1];
             [41;85;79;0;51;50;89;3;59;75;24;68;23;0;15;40;47;51;36;91];
             [9;18;40;58;60;90;11;14;59;1;39;9;78;21;32;50;78;81;45;3];
             [18;11;84;8;24;22;33;59;14;11;84;33;68;78;67;61;78;77;62;5];
             [54;60;91;72;76;84;7;83;79;12;72;44;70;15;24;87;90;75;37;84];
             [95;10;28;38;35;48;95;43;9;16;69;29;77;22;4;19;48;63;1;10];
             [65;14;24;42;21;3;37;34;92;0;21;2;41;23;17;53;39;24;85;56];
             [47;14;27;4;10;77;48;38;74;83;40;4;4;28;0;52;65;18;3;16];
             [7;17;11;2;87;1;42;47;2;78;20;98;89;11;75;60;21;82;57;80];
             [72;43;68;4;6;13;9;37;14;0;27;66;94;55;22;16;65;57;87;11];
             [44;54;69;93;26;98;10;25;62;73;98;85;13;82;71;21;37;73;13;86];
             [84;58;71;3;66;70;56;90;0;98;61;12;26;18;83;31;82;24;70;20];
             [17;28;47;62;59;7;66;12;30;5;33;82;2;86;19;78;91;34;68;59];
             [35;3;22;10;77;60;60;2;75;79;3;14;96;32;54;67;97;24;28;61];
             [40;85;83;94;34;99;98;65;34;15;79;1;10;77;24;13;88;81;38;68];
             [13;53;80;86;70;77;2;71;68;73;66;8;15;51;39;92;6;5;31;37];
             [4;18;63;18;39;48;83;95;66;4;4;23;55;14;18;49;60;33;0;53];
             [31;25;31;19;39;33;52;96;42;24;74;12;91;25;72;14;88;96;49;51];
             [17;60;42;41;74;2;41;67;0;20;66;44;36;53;66;35;14;8;59;76];
             [82;46;24;52;68;15;37;24;41;17;94;56;69;32;71;13;95;98;1;31];
             [8;12;68;20;81;63;26;4;94;87;21;66;12;44;31;96;81;24;25;74];
             [42;43;85;13;64;16;11;12;70;98;35;29;36;81;36;85;67;50;17;37];
             [61;65;65;78;63;30;95;37;23;72;33;24;68;84;40;38;3;6;1;40];
             [22;30;88;84;30;55;25;6;15;12;57;75;50;66;4;21;41;91;32;81];
             [27;23;63;14;57;40;58;41;98;35;76;63;46;80;25;3;32;59;18;88];
             [50;58;85;71;57;39;53;54;53;45;48;4;95;89;64;31;59;78;72;26];
             [4;51;53;94;92;72;61;8;76;5;59;51;17;53;6;90;20;19;78;52];
             [55;97;83;76;33;0;74;54;53;46;91;85;59;35;78;68;33;67;31;29];
             [36;34;98;64;44;80;27;54;11;25;99;39;43;73;40;35;65;71;33;94];
             [31;39;23;87;96;14;75;68;62;57;27;71;21;78;37;8;5;32;61;96];
             [55;60;35;10;71;31;85;40;97;68;88;22;47;80;68;89;88;67;54;54];
             [42;94;84;62;96;96;52;7;96;81;57;41;87;97;2;35;75;85;82;78];
             [73;18;83;10;36;26;46;80;2;94;66;63;0;79;27;19;15;1;8;17];
             [70;60;98;16;31;67;37;83;20;96;86;28;99;32;80;78;15;20;33;54];
             [8;63;29;4;98;54;2;56;4;15;54;8;82;27;22;58;7;17;37;51];
             [36;67;22;2;30;91;95;60;6;45;33;91;96;57;38;30;68;86;94;52];
             [88;51;98;76;70;62;9;88;23;27;1;49;48;15;38;39;21;91;63;56];
             [87;0;27;62;46;40;18;71;91;83;75;7;85;60;76;31;41;7;4;75];
             [93;49;96;58;79;3;17;57;63;26;84;57;9;44;78;79;93;69;74;56];
             [19;37;85;53;60;31;48;25;83;72;35;93;86;79;36;55;61;74;19;96];
             [69;87;64;77;39;79;8;66;49;21;96;91;78;23;95;89;71;30;89;7];
             [90;37;11;74;70;34;93;25;69;88;38;41;37;22;46;77;29;6;17;23];
             [59;25;28;47;12;92;68;4;46;96;41;34;2;68;38;67;24;16;18;54];
             [62;47;16;5;65;38;34;54;71;37;36;66;66;4;22;81;89;1;25;80];
             [24;17;52;22;86;47;2;22;6;28;16;5;93;67;50;87;54;36;71;80];
             [21;32;96;75;40;50;59;82;13;26;3;48;43;68;66;46;71;65;48;78];
             [25;5;94;86;50;19;9;72;75;95;68;98;1;58;34;36;1;21;62;44];
             [0;94;92;96;99;73;37;90;94;15;43;52;42;21;14;31;36;68;78;11];
             [58;53;56;8;0;0;25;28;50;35;17;87;11;1;75;77;4;44;80;22];
             [59;22;35;48;50;82;87;70;12;26;43;29;52;68;88;27;43;8;46;29];
             [12;67;37;44;86;47;28;15;67;89;26;1;73;37;51;38;44;17;76;41];
             [53;69;6;44;55;52;94;72;11;95;20;42;24;65;73;1;19;65;33;98];
             [89;9;76;36;56;9;30;27;27;82;14;96;34;46;73;49;66;69;82;21];
             [85;47;95;71;0;48;1;10;91;3;25;88;24;69;8;68;51;42;81;4];
             [67;95;18;81;24;18;60;77;13;76;24;12;39;12;8;19;77;92;42;74];
             [24;3;47;89;30;57;94;82;63;11;53;31;89;4;74;72;68;40;93;43];
             [71;47;61;84;9;23;42;18;22;72;29;19;50;34;14;47;5;60;58;31];
             [17;79;1;29;95;71;25;64;46;39;30;27;62;29;86;78;76;22;71;78];
             [12;22;64;60;81;70;2;2;8;46;9;0;3;85;34;59;52;60;49;77];
             [27;14;48;78;54;75;66;69;78;98;84;47;90;8;63;17;77;19;43;3];
             [64;38;94;64;20;38;19;69;16;63;67;74;71;53;83;56;66;81;58;7];
             [43;78;9;95;20;4;50;62;78;23;91;7;58;86;48;81;54;3;61;78];
             [90;22;14;83;28;25;68;18;1;82;95;6;17;28;24;60;11;77;16;22];
             [44;20;40;88;41;94;27;80;16;82;35;35;12;93;75;30;79;5;10;44];
             [47;12;11;45;10;11;87;13;99;88;52;78;39;59;79;23;18;84;99;73];
             [67;39;53;27;57;42;3;71;58;35;97;33;88;53;39;31;71;33;79;12];
             [94;91;26;33;52;58;15;88;30;58;15;25;17;7;23;38;52;9;28;30];
             [25;21;46;19;97;88;64;77;41;35;66;26;31;21;44;6;38;43;74;21];
             [85;42;84;94;21;31;86;66;92;63;21;70;66;52;62;47;13;99;93;45];
             [41;8;77;20;0;6;96;36;96;43;74;73;98;89;81;68;80;86;6;64];
             [25;4;97;68;73;7;32;37;63;47;63;45;41;86;84;91;34;48;72;2];
             [31;19;46;0;23;15;29;29;32;89;36;91;35;67;88;11;3;89;96;8];
             [69;42;51;22;42;80;96;81;17;68;21;44;5;47;23;23;9;71;63;17];
             [29;49;37;62;11;4;30;90;86;19;34;56;42;19;13;72;19;29;80;14];
             [49;71;95;33;85;48;86;61;42;43;19;27;43;1;59;37;62;12;29;64];
             [42;80;5;18;67;27;47;70;92;97;90;29;58;84;92;58;37;91;66;22];
             [20;77;20;5;12;18;10;69;89;6;0;18;23;97;67;20;19;41;12;85];
             [21;0;36;23;55;27;85;85;49;70;60;40;95;66;30;44;43;35;56;99];
             [59;11;63;14;96;98;11;69;11;38;62;14;47;5;48;1;5;76;39;47];
             [67;90;27;1;58;78;71;86;88;33;59;37;39;25;66;42;88;7;66;4];
             [73;46;38;4;37;80;43;85;67;30;47;46;43;7;28;78;86;79;43;24];
             [85;23;25;32;82;9;12;44;98;42;92;28;80;19;55;27;24;93;42;71];
             [15;93;6;59;94;37;96;24;74;31;85;67;33;16;89;57;42;45;62;64];
             [83;16;97;49;65;11;24;38;83;39;42;27;92;31;16;49;89;49;73;10];
             [18;19;53;93;21;85;6;25;84;0;26;11;90;59;66;82;41;58;47;20];
             [52;28;54;92;43;96;90;82;2;34;80;54;9;98;85;79;27;51;11;75];
             [8;5;0;30;63;63;60;79;50;57;32;39;2;92;56;93;82;19;44;77];
             [38;72;78;71;2;55;66;1;19;55;77;45;8;31;3;34;73;6;56;93];
             [22;66;51;73;17;80;62;74;32;67;14;2;89;74;8;66;49;12;70;50];
             [99;54;68;94;68;76;37;93;1;54;54;94;61;28;8;39;85;92;76;6];
             [22;95;21;48;72;96;81;3;46;77;82;57;98;32;96;18;44;63;14;39];
             [16;49;40;18;33;63;18;65;99;13;16;22;83;82;45;85;74;60;39;28];
             [65;95;82;51;3;10;37;82;68;31;28;18;48;12;44;47;79;92;22;64];
             [74;2;58;53;73;81;3;12;54;29;43;11;28;23;94;22;11;48;97;28];
             [10;91;20;28;68;52;39;96;11;38;54;61;31;74;79;29;94;58;17;30];
             [75;85;70;89;81;27;20;81;7;85;78;35;97;8;80;40;43;32;86;13];
             [50;29;89;85;22;14;98;19;63;80;10;61;1;18;0;92;99;12;44;81];
             [0;50;67;92;76;4;96;59;14;88;44;53;58;89;4;12;55;32;53;10];
             [15;41;5;29;7;27;22;16;64;2;49;62;40;23;60;26;68;30;19;33];
             [29;62;19;65;30;23;21;97;43;8;35;2;52;51;96;30;24;59;3;15];
             [16;35;22;74;44;17;15;59;48;65;79;10;77;1;68;14;88;39;68;75];
             [28;22;44;3;17;11;28;0;57;3;10;91;90;3;35;27;13;52;58;68];
             [95;78;4;28;30;59;4;52;48;76;89;16;11;54;80;86;76;83;28;43];
             [35;74;88;55;87;14;0;90;32;88;96;68;60;20;17;66;30;46;23;87];
             [18;18;9;39;12;80;30;1;70;13;8;28;10;70;41;26;36;62;52;43];
             [46;12;15;70;53;58;15;68;30;88;12;18;16;25;22;48;55;24;29;85];
             [44;98;9;53;16;32;25;73;7;81;4;11;50;77;89;87;1;42;0;81];
             [80;15;44;37;62;2;23;12;45;59;62;16;8;29;69;21;14;30;0;47];
             [64;75;87;3;72;74;67;71;91;13;62;21;38;8;29;58;49;1;94;22];
             [59;76;68;83;80;9;65;12;18;74;14;43;32;9;93;81;6;63;69;62];
             [69;96;10;91;61;48;28;64;70;74;36;50;21;35;18;26;46;41;27;63];
             [42;12;78;76;12;24;63;94;40;80;42;98;39;60;9;80;61;19;51;35];
             [92;98;47;80;83;59;16;28;65;9;64;51;3;61;9;4;9;37;97;85];
             [20;16;95;55;28;24;29;10;16;15;50;88;8;51;65;95;14;43;17;49];
             [79;73;70;49;69;79;23;52;45;34;16;34;53;36;21;34;30;53;1;42];
             [74;81;95;32;78;92;23;12;56;33;73;7;99;88;88;66;1;63;76;89];
             [87;84;74;83;14;56;52;49;40;35;38;40;18;10;93;7;45;59;47;4];
             [35;13;73;65;15;28;45;4;85;93;80;29;43;52;73;92;70;97;39;72]]
