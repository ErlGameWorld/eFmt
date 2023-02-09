eFmt
=====

An OTP library to format term, for efficient base on otp-25.2.1


Build
-----
    $ rebar3 compile

# 说明
## 格式 
    ~F.P.PadModC  其中 C是唯一必填字段  F, P, Pad和Mod是可选的
    F是打印参数的字段宽度。负值表示参数在字段内左对齐，否则右对齐。如果未指定字段宽度，则使用所需的打印宽度。如果指定的字段宽度太小，则整个字段都用*字符填充。
    P是打印参数的精度。如果未指定精度，则使用默认值。精度的解释取决于控制序列。除非另有说明，参数within用于确定打印宽度。
    Pad是填充字符。这是用于填充参数的打印表示的字符，以使其符合指定的字段宽度和精度。只能指定一个填充字符，并且在适用的情况下，它用于字段宽度和精度。默认的填充字符是' '（空格）。
    Mod是控制序列修饰符。这是一个或多个改变数据解释的字符 。当前的修饰符是t，用于 Unicode 翻译，l，用于阻止p和P 检测可打印字符。
    如果如果F、P或Pad是*字符，则Data中的下一个参数用作值。例如：io:fwrite("~*.*.0f~n",[9, 5, 3.14159265]).  -> 003.14159
    要将文字*字符用作Pad，它必须作为参数传递：io:fwrite("~*.*.*f~n",[9, 5, $*, 3.14159265]).  -> **3.14159

## Available control sequences:
```
~ Character ~ is written.

c 参数是一个被解释为 ASCII 代码的数字。精度是打印字符的次数，默认为字段宽度，而字段宽度又默认为 1. Example:
    1> io:fwrite("|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]).   -> |     aaaaa|bbbbb     |ccccc|
    如果 Unicode 转换修饰符 ( t ) 有效，整数参数可以是代表有效 Unicode 代码点的任何数字，否则它是一个小于或等于 255 的整数，否则它被 16#FF 屏蔽：
    2> io:fwrite("~tc~n",[1024]).
    \x{400}
    ok
    3> io:fwrite("~c~n",[1024]).
    ^@
    ok

f 参数是一个浮点数，写为 [-]ddd.ddd，其中精度是小数点后的位数。默认精度为 6，不能小于 1。

e 参数是一个浮点数，写为 [-]d.ddde+-ddd，其中精度是写入的位数。默认精度为 6，不能小于 2。

g 参数是一个浮点数，如果它 >= 0.1 且 < 10000.0 ，则写为f 。否则，以e格式写入。精度是有效数字的个数。默认为6，不会<2。如果float的绝对值不允许用f格式写成想要的有效位数，也写成e格式。

s 使用字符串语法打印参数。参数是，如果不存在 Unicode 转换修饰符， 则为 iolist()、binary()或atom()。如果 Unicode 转换修饰符 ( t ) 有效，则参数为unicode:chardata()，这意味着二进制文件采用 UTF-8。打印的字符不带引号。该字符串首先按指定的精度截断，然后填充并对齐到指定的字段宽度。默认精度是字段宽度。此格式可用于打印任何对象并截断输出以适合指定字段：
    1> io:fwrite("|~10w|~n", [{hey, hey, hey}]).
    |**********|
    ok
    2> io:fwrite("|~10s|~n", [io_lib:write({hey, hey, hey})]).
    |{hey,hey,h|
    3> io:fwrite("|~-10.8s|~n", [io_lib:write({hey, hey, hey})]).
    |{hey,hey  |
    ok
    如果未指定 Unicode 转换修饰符，则整数列表 > 255 被视为错误：
    4> io:fwrite("~ts~n",[[1024]]).
    \x{400}
    ok
    5> io:fwrite("~s~n",[[1024]]).
    ** exception error: bad argument
    in function  io:format/3
    called as io:format(<0.53.0>,"~s~n",[[1024]])

w 使用标准语法写入数据。这用于输出 Erlang 术语。如果原子包含嵌入的不可打印字符，则它们将打印在引号内。除非使用 Unicode 转换修饰符 ( t ) ，否则将转义大于 255 的原子字符。浮点数被准确打印为最短、正确舍入的字符串。

p 以与~w相同的方式使用标准语法写入数据 ，但将打印表示长于一行的术语分成多行，并明智地缩进每行。不支持左对齐。它还尝试检测可打印字符的平面列表并将它们输出为字符串。例如：
    1> T = [{attributes,[[{id,age,1.50000},{mode,explicit},
    {typename,"INTEGER"}], [{id,cho},{mode,explicit},{typename,'Cho'}]]},
    {typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}].
    ...
    2> io:fwrite("~w~n", [T]).
    [{attributes,[[{id,age,1.5},{mode,explicit},{typename,
    [73,78,84,69,71,69,82]}],[{id,cho},{mode,explicit},{typena
    me,'Cho'}]]},{typename,'Person'},{tag,{'PRIVATE',3}},{mode
    ,implicit}]
    ok
    3> io:fwrite("~62p~n", [T]).
    [{attributes,[[{id,age,1.5},
    {mode,explicit},
    {typename,"INTEGER"}],
    [{id,cho},{mode,explicit},{typename,'Cho'}]]},
    {typename,'Person'},
    {tag,{'PRIVATE',3}},
    {mode,implicit}]
    ok
    字段宽度指定最大行长度。它默认为 80。精度指定术语的初始缩进。它默认为在对write/1或 format/1,2,3的同一调用中 打印在此行的字符数。例如，使用上面的T：
    
    4> io:fwrite("Here T = ~62p~n", [T]).
    Here T = [{attributes,[[{id,age,1.5},
    {mode,explicit},
    {typename,"INTEGER"}],
    [{id,cho},
    {mode,explicit},
    {typename,'Cho'}]]},
    {typename,'Person'},
    {tag,{'PRIVATE',3}},
    {mode,implicit}]
    ok
    从 Erlang/OTP 21.0 开始，值为 0的字段宽度可用于指定一行无限长，这意味着不插入换行符。例如：
    5> io:fwrite("~0p~n", [lists:seq(1, 30)]).
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
    ok
    当指定修饰符l时，不会检测可打印字符列表，例如：
    6> S = [{a,"a"}, {b, "b"}],
    io:fwrite("~15p~n", [S]).
    [{a,"a"},
    {b,"b"}]
    ok
    7> io:fwrite("~15lp~n", [S]).
    [{a,[97]},
    {b,[98]}]
    ok
    Unicode 翻译修饰符t指定如何处理 Latin-1 代码点范围之外的字符，在原子、字符串和二进制文件中。例如，打印一个包含字符 > 255 的原子：
    8> io:fwrite("~p~n",[list_to_atom([1024])]).
    '\x{400}'
    ok
    9> io:fwrite("~tp~n",[list_to_atom([1024])]).
    'Ѐ'
    ok
    默认情况下，Erlang 仅将 Latin-1 范围内的字符列表检测为字符串，但+pc unicode 标志可用于更改此设置（有关详细信息，请参阅printable_range/0）。例如：
    10> io:fwrite("~p~n",[[214]]).
    "Ö"
    ok
    11> io:fwrite("~p~n",[[1024]]).
    [1024]
    ok
    12> io:fwrite("~tp~n",[[1024]]).
    [1024]
    ok
    but if Erlang was started with +pc unicode:
    
    13> io:fwrite("~p~n",[[1024]]).
    [1024]
    ok
    14> io:fwrite("~tp~n",[[1024]]).
    "Ѐ"
    ok
    同样，如果指定了t 修饰符，则看起来像 UTF-8 编码字符串的二进制文件将使用二进制字符串语法输出：Similarly, binaries that look like UTF-8 encoded strings are output with the binary string syntax if the t modifier is specified:
    
    15> io:fwrite("~p~n", [<<208,128>>]).
    <<208,128>>
    ok
    16> io:fwrite("~tp~n", [<<208,128>>]).
    <<"Ѐ"/utf8>>
    ok
    17> io:fwrite("~tp~n", [<<128,128>>]).
    <<128,128>>
    ok
    
W 以与~w相同的方式写入数据，但需要一个额外的参数，即打印术语的最大深度。低于此深度的任何内容都将替换为 ...。例如，使用上面的T：
    8> io:fwrite("~W~n", [T,9]).
    [{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],
    [{id,cho},{mode,...},{...}]]},{typename,'Person'},
    {tag,{'PRIVATE',3}},{mode,implicit}]
    ok
    如果达到最大深度，则无法在结果输出中读取。此外，元组中的 ,...形式表示元组中有更多元素，但这些元素低于打印深度。

P 以与~p相同的方式写入数据，但需要一个额外的参数，即打印术语的最大深度。低于此深度的任何内容都将替换为 ...，例如：
    9> io:fwrite("~62P~n", [T,9]).
    [{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],
    [{id,cho},{mode,...},{...}]]},
    {typename,'Person'},
    {tag,{'PRIVATE',3}},
    {mode,implicit}]
    ok
    
B 以 2-36 为基数写入整数，默认基数为 10。负整数会打印前导破折号。
    precision字段选择base，例如：
    1> io:fwrite("~.16B~n", [31]).
    1F
    ok
    2> io:fwrite("~.2B~n", [-19]).
    -10011
    ok
    3> io:fwrite("~.36B~n", [5*36+35]).
    5Z
    ok
    
X 与B类似，但需要一个额外的参数，该参数是要在数字之前插入的前缀，但在前导破折号之后（如果有的话）。
    1> io:fwrite("~X~n", [31,"10#"]).
    10#31
    ok
    2> io:fwrite("~.16X~n", [-31,"0x"]).
    -0x1F
    ok
    
# 与B类似，但使用 Erlang 样式#分隔的基本前缀打印数字 。例子：
    1> io:fwrite("~.10#~n", [31]).
    10#31
    ok
    2> io:fwrite("~.16#~n", [-31]).
    -16#1F
    ok
b 与B类似，但打印小写字母。

x 像X，但打印小写字母。

+ 像#，但打印小写字母。

n 写一个新行。

i 忽略下一个term。


