%% pretty 模式下 每行打印的字符数
-define(LineCCnt, 120).

-define(base(Precision), case Precision of none -> 10; _ -> Precision end).
%% 三元表达式
-define(IIF(Cond, Ret1, Ret2), (case Cond of true -> Ret1; _ -> Ret2 end)).

-record(fmtSpec, {
   ctlChar :: char()                         %% 控制序列的类型  $p $w
   , args :: [any()]                         %% 是控制序列使用的参数的列表，如果控制序列不带任何参数，则为空列表。
   , width :: 'none' | integer()             %% 字段宽度
   , adjust :: 'left' | 'right'              %% 对齐方式
   , precision :: 'none' | integer()         %% 打印参数的精度
   , padChar :: char()                       %% 填充字符
   , encoding :: 'unicode' | 'latin1'        %% 如果存在翻译修饰符t，则编码设置为true
   , strings :: boolean()                    %% 如果存在修饰符l，则将 string设置为false。
}).

-define(log(Args), io:format("IMY**********~w~n", [Args])).


