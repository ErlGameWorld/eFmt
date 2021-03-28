-module(eFmt).

-import(binary, [split/2, part/3]).
-import(lists, [keyfind/3, reverse/2]).
-import(maps, [iterator/1, next/1]).

-compile(inline).
-compile({inline_size, 128}).

-include("eFmt.hrl").

-export([
   %% eFmt
   format/2
   , format/3

   , formatBin/2
   , formatBin/3

   , scan/2
   , build/1
   , build/2

   , write/1
   , write/2
   , write/3
   , write/4
   , write/6

   %% eFmtformat
   , fWrite/2
   , fWrite/3
   , fScan/2
   , fBuild/1
   , fBuild/2

   %% utils
   , toLowerStr/1
   , toUpperStr/1
   , toBinary/1
   , charsLen/1
   , getOpt/3
   , visualList/2
   , visualBin/2
   , writeTerm/3
   , writeTerm/5
]).

%% ********************************************** eFmt start ***********************************************************
-type chars() :: [char() | chars() | binary()].
-type depth() :: -1 | non_neg_integer().
-type encoding() :: epp:source_encoding() | 'unicode'.
-type charsLimit() :: integer().
-type fmtSpec() :: #fmtSpec{}.
-type format() :: atom() | string() | binary().

-spec format(Format :: format(), Data :: [term()]) -> chars().
format(Format, Args) ->
   try fWrite(Format, Args)
   catch
      _C:_R ->
         erlang:error(badarg, [Format, Args, _C, _R])
   end.

-spec format(Format :: format(), Data :: [term()], Options :: [{charsLimit, CharsLimit :: charsLimit()}]) -> chars().
format(Format, Args, Options) ->
   try fWrite(Format, Args, Options)
   catch
      _C:_R ->
         erlang:error(badarg, [Format, Args])
   end.

-spec formatBin(Format :: format(), Data :: [term()]) -> chars().
formatBin(Format, Args) ->
   try iolist_to_binary(fWrite(Format, Args))
   catch
      _C:_R ->
         erlang:error(badarg, [Format, Args, _C, _R])
   end.

-spec formatBin(Format :: format(), Data :: [term()], Options :: [{charsLimit, CharsLimit :: charsLimit()}]) -> chars().
formatBin(Format, Args, Options) ->
   try iolist_to_binary(fWrite(Format, Args, Options))
   catch
      _C:_R ->
         erlang:error(badarg, [Format, Args])
   end.

-spec scan(Format :: format(), Data :: [term()]) -> FormatList :: [char() | fmtSpec()].
scan(Format, Args) ->
   try fScan(Format, Args)
   catch
      _C:_R ->
         erlang:error(badarg, [Format, Args])
   end.

-spec build(FormatList :: [char() | fmtSpec()]) -> chars().
build(FormatList) ->
   try fBuild(FormatList)
   catch
      _C:_R ->
         erlang:error(badarg, [FormatList])
   end.

-spec build(FormatList :: [char() | fmtSpec()], Options :: [{charsLimit, CharsLimit :: charsLimit()}]) -> chars().
build(FormatList, Options) ->
   try fBuild(FormatList, Options)
   catch
      _C:_R ->
         erlang:error(badarg, [FormatList, Options])
   end.

-spec write(Term :: term()) -> chars().
write(Term) ->
   writeTerm(Term, -1, latin1).

-spec write(Term :: term(), Depth :: depth()) -> chars().
write(Term, Depth) ->
   writeTerm(Term, Depth, latin1).

-spec write(Term :: term(), Depth :: depth(), IsPretty :: boolean()) -> chars().
write(Term, Depth, IsPretty) ->
   case IsPretty of
      true ->
         writeTerm(Term, Depth, ?LineCCnt, latin1, true);
      _ ->
         writeTerm(Term, Depth, latin1)
   end.

-spec write(Term :: term(), Depth :: depth(), Encoding :: encoding(), CharsLimit :: charsLimit()) -> chars().
write(Term, Depth, Encoding, CharsLimit) ->
   if
      Depth =:= 0 orelse CharsLimit =:= 0 ->
         <<"...">>;
      CharsLimit < 0 ->
         writeTerm(Term, Depth, Encoding);
      true ->
         BinTerm = writeTerm(Term, Depth, ?LineCCnt, Encoding, false),
         BinTermSize = byte_size(BinTerm),
         if
            CharsLimit < 0 ->
               BinTerm;
            BinTermSize > CharsLimit ->
               <<(part(BinTerm, 0, CharsLimit))/binary, "...">>;
            true ->
               BinTerm
         end
   end.

write(Term, Depth, Width, CharsLimit, Encoding, Strings) ->
   if
      Depth =:= 0 orelse CharsLimit =:= 0 ->
         <<"...">>;
      true ->
         BinTerm = writeTerm(Term, Depth, Width, Encoding, Strings),
         BinTermSize = byte_size(BinTerm),
         if
            CharsLimit < 0 ->
               BinTerm;
            BinTermSize > CharsLimit ->
               <<(part(BinTerm, 0, CharsLimit))/binary, "...">>;
            true ->
               BinTerm
         end
   end.

-define(writeInt(Int), integer_to_binary(Term)).
-define(writeFloat(Float), floatG(Term)).
-define(writePort(Port), list_to_binary(port_to_list(Port))).
-define(writeRef(Ref), list_to_binary(ref_to_list(Ref))).
-define(writePid(Ref), list_to_binary(pid_to_list(Ref))).
-define(writeFun(Fun), list_to_binary(erlang:fun_to_list(Fun))).

writeAtom(Atom, Encoding) ->
   AtomBin = atom_to_binary(Atom, Encoding),
   case isQuoteAtom(Atom, AtomBin) of
      true ->
         <<"'", (atom_to_binary(Atom, Encoding))/binary, "'">>;
      _ ->
         AtomBin
   end.

isQuoteAtom(Atom, AtomBin) ->
   case erl_scan:reserved_word(Atom) of
      true -> true;
      _ ->
         visualAtomBin(AtomBin)
   end.

visualAtomBin(<<>>) -> false;
visualAtomBin(<<C/utf8, Left/binary>>) -> ?IIF(visualAtomChar(C), visualAtomBin(Left), true);
visualAtomBin(_) -> true.

visualAtomChar(C) when C >= $a, C =< $z -> true;
visualAtomChar(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
visualAtomChar(C) when C >= $A, C =< $Z -> true;
visualAtomChar(C) when C >= $À, C =< $Þ, C =/= $× -> true;
visualAtomChar(C) when C >= $0, C =< $9 -> true;
visualAtomChar($_) -> true;
visualAtomChar($@) -> true;
visualAtomChar(_) -> false.

%% **************************************************** ~w start *******************************************************
writeList([], _D, _E, BinAcc) ->
   <<BinAcc/binary, "]">>;
writeList([One], D, E, BinAcc) ->
   VBin = writeTerm(One, D, E),
   <<BinAcc/binary, VBin/binary, "]">>;
writeList([One | List], D, E, BinAcc) ->
   if
      D =:= 1 -> <<BinAcc, "|...]">>;
      true ->
         VBin = writeTerm(One, D, E),
         writeList(List, D - 1, E, <<BinAcc/binary, VBin/binary, ",">>)
   end;
writeList(Other, D, E, BinAcc) ->
   NewBinAcc = part(BinAcc, 0, byte_size(BinAcc) - 1),
   VBin = writeTerm(Other, D, E),
   <<NewBinAcc/binary, "|", VBin/binary, "]">>.

writeTuple(Tuple, D, E, Index, TupleSize, BinAcc) ->
   if
      D =:= 1 -> <<BinAcc/binary, "...}">>;
      true ->
         if
            Index < TupleSize ->
               VBin = writeTerm(element(Index, Tuple), D - 1, E),
               writeTuple(Tuple, D - 1, E, Index + 1, TupleSize, <<BinAcc/binary, VBin/binary, ",">>);
            Index == TupleSize ->
               VBin = writeTerm(element(Index, Tuple), D - 1, E),
               <<BinAcc/binary, VBin/binary, "}">>;
            true ->
               <<BinAcc/binary, "}">>
         end
   end.

writeMap(Map, D, E, BinAcc) ->
   if
      D =:= 1 ->
         <<BinAcc/binary, "...}">>;
      true ->
         writeMapBody(iterator(Map), D, E, BinAcc)
   end.

writeMapBody(I, D, E, BinAcc) ->
   if
      D =:= 1 ->
         <<BinAcc/binary, " ...}">>;
      true ->
         case next(I) of
            {K, V, none} ->
               KeyTermBin = writeTerm(K, -1, E),
               ValueTermBin = writeTerm(V, -1, E),
               <<BinAcc/binary, KeyTermBin/binary, " => ", ValueTermBin/binary, "}">>;
            {K, V, NextI} ->
               KeyTermBin = writeTerm(K, -1, E),
               ValueTermBin = writeTerm(V, -1, E),
               writeMapBody(NextI, D - 1, E, <<BinAcc/binary, KeyTermBin/binary, " => ", ValueTermBin/binary, ",">>);
            _ ->
               <<BinAcc/binary, "}">>
         end
   end.

writeBinary(Bin, D, BinAcc) ->
   if
      D == 1 ->
         <<BinAcc/binary, "...>>">>;
      true ->
         case Bin of
            <<>> ->
               <<BinAcc/binary, ">>">>;
            <<Int:8>> ->
               VBin = integer_to_binary(Int),
               <<BinAcc/binary, VBin/binary, ">>">>;
            <<Int:8, LeftBin/bitstring>> ->
               VBin = integer_to_binary(Int),
               writeBinary(LeftBin, D - 1, <<BinAcc/binary, VBin/binary, ",">>);
            _ ->
               L = bit_size(Bin),
               <<X:L>> = Bin,
               XBin = integer_to_binary(X),
               LBin = integer_to_binary(L),
               <<BinAcc/binary, XBin/binary, ":", LBin/binary, ">>">>
         end
   end.

%% **************************************************** ~w end   *******************************************************

%% **************************************************** ~p start *******************************************************
writeList([], _Depth, _Width, _Encoding, _Strings) ->
   <<"[]">>;
writeList(List, Depth, Width, Encoding, Strings) ->
   case Strings andalso visualList(List, Encoding) of
      true ->
         <<"\"", (unicode:characters_to_binary(List))/binary, "\"">>;
      _ ->
         writeList(List, Depth, Width, Encoding, Strings, 0, <<"[">>)
   end.

writeList([], _Depth, _Width, _Encoding, _Strings, _SumLC, BinAcc) ->
   <<BinAcc/binary, "]">>;
writeList([One], Depth, Width, Encoding, Strings, SumLC, BinAcc) ->
   TermBin = writeTerm(One, Depth, Width, Encoding, Strings),
   TermBinBinSize = byte_size(TermBin),
   NewSumLC = SumLC + TermBinBinSize,
   case NewSumLC >= Width of
      true ->
         <<BinAcc/binary, TermBin/binary, "]\n">>;
      _ ->
         <<BinAcc/binary, TermBin/binary, "]">>

   end;
writeList([One | List], Depth, Width, Encoding, Strings, SumLC, BinAcc) ->
   if
      Depth =:= 1 -> <<BinAcc, "|...]">>;
      true ->
         TermBin = writeTerm(One, Depth, Width, Encoding, Strings),
         TermBinBinSize = byte_size(TermBin),
         NewSumLC = SumLC + TermBinBinSize,
         case NewSumLC >= Width of
            true ->
               writeList(List, Depth - 1, Width, Encoding, Strings, 0, <<BinAcc/binary, TermBin/binary, ",\n">>);
            _ ->
               writeList(List, Depth - 1, Width, Encoding, Strings, NewSumLC, <<BinAcc/binary, TermBin/binary, ",">>)
         end
   end;
writeList(Other, Depth, Width, Encoding, Strings, SumLC, BinAcc) ->
   TermBin = writeTerm(Other, Depth, Width, Encoding, Strings),
   TermBinBinSize = byte_size(TermBin),
   NewSumLC = SumLC + TermBinBinSize,
   NewBinAcc = part(BinAcc, 0, byte_size(BinAcc) - 1),
   case NewSumLC >= Width of
      true ->
         <<NewBinAcc/binary, "|", TermBin/binary, "]\n">>;
      _ ->
         <<NewBinAcc/binary, "|", TermBin/binary, "]">>
   end.

writeTuple(Tuple, Depth, Width, Encoding, Strings, Index, TupleSize, SumLC, BinAcc) ->
   if
      Depth =:= 1 -> <<BinAcc/binary, "...}">>;
      true ->
         if
            Index < TupleSize ->
               TermBin = writeTerm(element(Index, Tuple), Depth, Width, Encoding, Strings),
               TermBinBinSize = byte_size(TermBin),
               NewSumLC = SumLC + TermBinBinSize,
               case NewSumLC >= Width of
                  true ->
                     writeTuple(Tuple, Depth - 1, Width, Encoding, Strings, Index + 1, TupleSize, 0, <<BinAcc/binary, TermBin/binary, ",\n">>);
                  _ ->
                     writeTuple(Tuple, Depth - 1, Width, Encoding, Strings, Index + 1, TupleSize, NewSumLC, <<BinAcc/binary, TermBin/binary, ",">>)
               end;
            Index == TupleSize ->
               TermBin = writeTerm(element(Index, Tuple), Depth, Width, Encoding, Strings),
               TermBinBinSize = byte_size(TermBin),
               NewSumLC = SumLC + TermBinBinSize,
               case NewSumLC >= Width of
                  true ->
                     <<BinAcc/binary, TermBin/binary, "}\n">>;
                  _ ->
                     <<BinAcc/binary, TermBin/binary, "}">>
               end;
            true ->
               <<BinAcc/binary, "}">>
         end
   end.

writeMap(Map, Depth, Width, Encoding, Strings, SumLC, BinAcc) ->
   if
      Depth =:= 1 ->
         <<BinAcc/binary, "...}">>;
      true ->
         writeMapBody(iterator(Map), Depth, Width, Encoding, Strings, SumLC, BinAcc)
   end.

writeMapBody(I, Depth, Width, Encoding, Strings, SumLC, BinAcc) ->
   if
      Depth =:= 1 ->
         <<BinAcc/binary, " ...}">>;
      true ->
         case next(I) of
            {K, V, none} ->
               KeyTermBin = writeTerm(K, -1, Width, Encoding, Strings),
               ValueTermBin = writeTerm(V, -1, Width, Encoding, Strings),
               TermBinBinSize = byte_size(KeyTermBin) + byte_size(ValueTermBin),
               NewSumLC = SumLC + TermBinBinSize,
               case NewSumLC >= Width of
                  true ->
                     <<BinAcc/binary, KeyTermBin/binary, " => ", ValueTermBin/binary, "}\n">>;
                  _ ->
                     <<BinAcc/binary, KeyTermBin/binary, " => ", ValueTermBin/binary, "}">>
               end;
            {K, V, NextI} ->
               KeyTermBin = writeTerm(K, -1, Width, Encoding, Strings),
               ValueTermBin = writeTerm(V, -1, Width, Encoding, Strings),
               TermBinBinSize = byte_size(KeyTermBin) + byte_size(ValueTermBin),
               NewSumLC = SumLC + TermBinBinSize,
               case NewSumLC >= Width of
                  true ->
                     writeMapBody(NextI, Depth - 1, Width, Encoding, Strings, 0, <<BinAcc/binary, KeyTermBin/binary, " => ", ValueTermBin/binary, ",\n">>);
                  _ ->
                     writeMapBody(NextI, Depth - 1, Width, Encoding, Strings, NewSumLC, <<BinAcc/binary, KeyTermBin/binary, " => ", ValueTermBin/binary, ",">>)
               end;
            _ ->
               <<BinAcc/binary, "}">>
         end
   end.

writeBinary(<<>>, _Depth, _Width, _Encoding, _Strings) ->
   <<"<<>>">>;
writeBinary(Bin, Depth, Width, Encoding, Strings) ->
   case Strings andalso visualBin(Bin, Encoding) of
      true ->
         <<"<<\"", Bin/binary, "\">>">>;
      _ ->
         writeBinary(Bin, Depth, Width, Encoding, Strings, 0, <<"<<">>)
   end.

writeBinary(Bin, Depth, Width, Encoding, Strings, SumLC, BinAcc) ->
   if
      Depth == 1 ->
         <<BinAcc/binary, "...>>">>;
      true ->
         case Bin of
            <<>> ->
               <<BinAcc/binary, ">>">>;
            <<Int:8>> ->
               <<BinAcc/binary, (integer_to_binary(Int))/binary, ">>">>;
            <<Int:8, LeftBin/bitstring>> ->
               TermBin = integer_to_binary(Int),
               TermBinBinSize = byte_size(TermBin),
               NewSumLC = SumLC + TermBinBinSize,
               case NewSumLC >= Width of
                  true ->
                     writeBinary(LeftBin, Depth - 1, Width, Encoding, Strings, 0, <<BinAcc/binary, TermBin/binary, ",\n">>);
                  _ ->
                     writeBinary(LeftBin, Depth - 1, Width, Encoding, Strings, NewSumLC, <<BinAcc/binary, TermBin/binary, ",">>)
               end;
            _ ->
               L = bit_size(Bin),
               <<X:L>> = Bin,
               <<BinAcc/binary, (integer_to_binary(X))/binary, ":", (integer_to_binary(L))/binary, ">>">>
         end
   end.
%% **************************************************** ~p end   *******************************************************

%% ~w
writeTerm(_Term, Depth, _E) when Depth == 0 -> <<"...">>;
writeTerm(Term, _Depth, _E) when is_integer(Term) -> ?writeInt(Term);
writeTerm(Term, _Depth, E) when is_atom(Term) -> writeAtom(Term, E);
writeTerm(Term, Depth, E) when is_list(Term) -> writeList(Term, Depth, E, <<"[">>);
writeTerm(Term, Depth, E) when is_map(Term) -> writeMap(Term, Depth, E, <<"#{">>);
writeTerm(Term, Depth, E) when is_tuple(Term) -> writeTuple(Term, Depth, E, 1, tuple_size(Term), <<"{">>);
writeTerm(Term, Depth, _E) when is_bitstring(Term) -> writeBinary(Term, Depth, <<"<<">>);
writeTerm(Term, _Depth, _E) when is_pid(Term) -> ?writePid(Term);
writeTerm(Term, _Depth, _E) when is_float(Term) -> ?writeFloat(Term);
writeTerm(Term, _Depth, _E) when is_port(Term) -> ?writePort(Term);
writeTerm(Term, _Depth, _E) when is_reference(Term) -> ?writeRef(Term);
writeTerm(Term, _Depth, _E) when is_function(Term) -> ?writeFun(Term).

%% ~p
writeTerm(_Term, Depth, _Width, _Encoding, _Strings) when Depth == 0 -> <<"...">>;
writeTerm(Term, _Depth, _Width, _Encoding, _Strings) when is_integer(Term) -> ?writeInt(Term);
writeTerm(Term, _Depth, _Width, Encoding, _Strings) when is_atom(Term) -> writeAtom(Term, Encoding);
writeTerm(Term, Depth, Width, Encoding, Strings) when is_list(Term) -> writeList(Term, Depth, Width, Encoding, Strings);
writeTerm(Term, Depth, Width, Encoding, Strings) when is_map(Term) ->
   writeMap(Term, Depth, Width, Encoding, Strings, 0, <<"#{">>);
writeTerm(Term, Depth, Width, Encoding, Strings) when is_tuple(Term) ->
   writeTuple(Term, Depth, Width, Encoding, Strings, 1, tuple_size(Term), 0, <<"{">>);
writeTerm(Term, Depth, Width, Encoding, Strings) when is_bitstring(Term) ->
   writeBinary(Term, Depth, Width, Encoding, Strings);
writeTerm(Term, _Depth, _Width, _Encoding, _Strings) when is_pid(Term) -> ?writePid(Term);
writeTerm(Term, _Depth, _Width, _Encoding, _Strings) when is_float(Term) -> ?writeFloat(Term);
writeTerm(Term, _Depth, _Width, _Encoding, _Strings) when is_port(Term) -> ?writePort(Term);
writeTerm(Term, _Depth, _Width, _Encoding, _Strings) when is_reference(Term) -> ?writeRef(Term);
writeTerm(Term, _Depth, _Width, _Encoding, _Strings) when is_function(Term) -> ?writeFun(Term).


%% ********************************************** eFmt end *************************************************************
%% ********************************************** eFmtFormat start *****************************************************
-spec fWrite(Format :: format(), Data :: [term()]) -> chars().
fWrite(Format, Args) ->
   fBuild(fScan(Format, Args), []).

-spec fWrite(Format :: format(), Data :: [term()], Options :: [{'chars_limit', CharsLimit :: integer()}]) -> chars().
fWrite(Format, Args, Options) ->
   fBuild(fScan(Format, Args), Options).

%% 格式 ~F.P.PadModC
%% Parse all control sequences in the format string.
-spec fScan(Format :: format(), Data :: [term()]) -> FormatList :: [char() | fmtSpec()].
fScan(Format, Args) ->
   if
      is_binary(Format) ->
         doCollect(Format, Args, []);
      is_list(Format) ->
         doCollect(list_to_binary(Format), Args, []);
      is_atom(Format) ->
         doCollect(atom_to_binary(Format, utf8), Args, []);
      true ->
         throw(bad_format)
   end.

doCollect(FmtBinStr, Args, Acc) ->
   case split(FmtBinStr, <<"~">>) of
      [NotMatch] ->
         true = [] == Args,
         ?IIF(NotMatch == <<>>, Acc, [NotMatch | Acc]);
      [FPart, LPart] ->
         doCollWidth(LPart, Args, 0, right, ?IIF(FPart == <<>>, Acc, [FPart | Acc]))
   end.

doCollWidth(<<>>, _Args, _Width, _Adjust, Acc) ->
   Acc;
doCollWidth(LPart, Args, Width, Adjust, Acc) ->
   case LPart of
      <<"-*", LeftLPart/binary>> ->
         [WidthArgs | LeftArgs] = Args,
         doCollPrecision(LeftLPart, LeftArgs, WidthArgs, left, Acc);
      <<"-", LeftLPart/binary>> ->
         doCollWidth(LeftLPart, Args, Width, left, Acc);
      <<"*", LeftLPart/binary>> ->
         [WidthArgs | LeftArgs] = Args,
         doCollPrecision(LeftLPart, LeftArgs, WidthArgs, right, Acc);
      <<WidthInt:8/integer, LeftLPart/binary>> ->
         case WidthInt >= $0 andalso WidthInt =< $9 of
            true ->
               doCollWidth(LeftLPart, Args, 10 * Width + (WidthInt - $0), Adjust, Acc);
            _ ->
               case Width == 0 of
                  true ->
                     doCollPrecision(LPart, Args, none, left, Acc);
                  _ ->
                     doCollPrecision(LPart, Args, Width, Adjust, Acc)
               end
         end
   end.

doCollPrecision(LPart, Args, Width, Adjust, Acc) ->
   case LPart of
      <<".", LeftLPart/binary>> ->
         doCollPrecision(LeftLPart, Args, Width, Adjust, 0, Acc);
      _ ->
         doCollPadChar(LPart, Args, Width, Adjust, none, Acc)
   end.

doCollPrecision(LPart, Args, Width, Adjust, Precision, Acc) ->
   case LPart of
      <<"*", LeftLPart/binary>> ->
         [PrecisionArgs | LeftArgs] = Args,
         doCollPadChar(LeftLPart, LeftArgs, Width, Adjust, PrecisionArgs, Acc);
      <<PrecisionInt:8/integer, LeftLPart/binary>> ->
         case PrecisionInt >= $0 andalso PrecisionInt =< $9 of
            true ->
               doCollPrecision(LeftLPart, Args, Width, Adjust, 10 * Precision + (PrecisionInt - $0), Acc);
            _ ->
               case Precision == 0 of
                  true ->
                     doCollPadChar(LPart, Args, Width, Adjust, none, Acc);
                  _ ->
                     doCollPadChar(LPart, Args, Width, Adjust, Precision, Acc)
               end
         end
   end.

doCollPadChar(LPart, Args, Width, Adjust, Precision, Acc) ->
   case LPart of
      <<".*", LeftLPart/binary>> ->
         [PadChar | LeftArgs] = Args,
         doCollEncoding(LeftLPart, LeftArgs, Width, Adjust, Precision, PadChar, Acc);
      <<".", PadChar:8/integer, LeftLPart/binary>> ->
         doCollEncoding(LeftLPart, Args, Width, Adjust, Precision, PadChar, Acc);
      _ ->
         doCollEncoding(LPart, Args, Width, Adjust, Precision, 32, Acc)
   end.

doCollEncoding(LPart, Args, Width, Adjust, Precision, PadChar, Acc) ->
   case LPart of
      <<"t", LeftLPart/binary>> ->
         %true = Char =/= $l,
         doCollStrings(LeftLPart, Args, Width, Adjust, Precision, PadChar, unicode, Acc);
      _ ->
         doCollStrings(LPart, Args, Width, Adjust, Precision, PadChar, latin1, Acc)
   end.

doCollStrings(LPart, Args, Width, Adjust, Precision, PadChar, Encoding, Acc) ->
   case LPart of
      <<"l", LeftLPart/binary>> ->
         %true = Char =/= $t,
         doCollCA(LeftLPart, Args, Width, Adjust, Precision, PadChar, Encoding, false, Acc);
      _ ->
         doCollCA(LPart, Args, Width, Adjust, Precision, PadChar, Encoding, true, Acc)
   end.

doCollCA(LPart, Args, Width, Adjust, Precision, PadChar, Encoding, Strings, Acc) ->
   <<CtlChar:8/integer, LeftLPart/binary>> = LPart,
   case CtlChar of
      $w -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $p -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $W -> [OneArgs | LeftArgs] = Args, [Depth | LastArgs] = LeftArgs, As = {OneArgs, Depth}, NextArgs = LastArgs;
      $P -> [OneArgs | LeftArgs] = Args, [Depth | LastArgs] = LeftArgs, As = {OneArgs, Depth}, NextArgs = LastArgs;
      $s -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $e -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $f -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $g -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $b -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $B -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $x -> [OneArgs | LeftArgs] = Args, [Prefix | LastArgs] = LeftArgs, As = {OneArgs, Prefix}, NextArgs = LastArgs;
      $X -> [OneArgs | LeftArgs] = Args, [Prefix | LastArgs] = LeftArgs, As = {OneArgs, Prefix}, NextArgs = LastArgs;
      $+ -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $# -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $c -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs;
      $~ -> As = undefined, NextArgs = Args;
      $n -> As = undefined, NextArgs = Args;
      $i -> [OneArgs | LeftArgs] = Args, As = OneArgs, NextArgs = LeftArgs
   end,
   FmtSpec = #fmtSpec{ctlChar = CtlChar, args = As, width = Width, adjust = Adjust, precision = Precision, padChar = PadChar, encoding = Encoding, strings = Strings},
   doCollect(LeftLPart, NextArgs, [FmtSpec | Acc]).

%% Build the output text for a pre-parsed format list.
-spec fBuild(FormatList :: [char() | fmtSpec()]) -> chars().
fBuild(Cs) ->
   fBuild(Cs, []).

-spec fBuild(FormatList :: [char() | fmtSpec()], Options :: [{'chars_limit', CharsLimit :: integer()}]) -> chars().
fBuild(Cs, Options) ->
   CharsLimit = getOpt(chars_limit, Options, -1),
   buildSmall(Cs, CharsLimit, 0, 0, 0, 0, []).

buildSmall([], CharsLimit, P, S, W, Other, Acc) ->
   NumOfLimited = P + S + W,
   case NumOfLimited of
      0 ->
         Acc;
      _ ->
         RemainChars = remainChars(CharsLimit, Other),
         case buildLimited(Acc, P, NumOfLimited, RemainChars, 0, []) of
            [] ->
               [];
            [_One] = Ret ->
               Ret;
            [One, Two] ->
               [Two, One];
            [One, Two, Three] ->
               [Three, Two, One];
            Ret ->
               reverse(Ret, [])
         end
   end;
buildSmall([OneCA | Cs], CharsLimit, P, S, W, Other, Acc) ->
   case OneCA of
      #fmtSpec{ctlChar = CtlChar, args = Args, width = Width, adjust = Adjust, precision = Precision, padChar = PadChar, encoding = Encoding} ->
         case ctlSmall(CtlChar, Args, Width, Adjust, Precision, PadChar, Encoding) of
            not_small ->
               case CtlChar of
                  $p ->
                     buildSmall(Cs, CharsLimit, P + 1, S, W, Other, [OneCA | Acc]);
                  $P ->
                     buildSmall(Cs, CharsLimit, P + 1, S, W, Other, [OneCA | Acc]);
                  $w ->
                     buildSmall(Cs, CharsLimit, P, S, W + 1, Other, [OneCA | Acc]);
                  $W ->
                     buildSmall(Cs, CharsLimit, P, S, W + 1, Other, [OneCA | Acc]);
                  $s ->
                     buildSmall(Cs, CharsLimit, P, S, W + 1, Other, [OneCA | Acc]);
                  _ ->
                     buildSmall(Cs, CharsLimit, P, S, W, Other, [OneCA | Acc])
               end;
            ignore ->
               buildSmall(Cs, CharsLimit, P, S, W, Other, Acc);
            Str ->
               if
                  is_binary(Str) orelse is_list(Str) ->
                     buildSmall(Cs, CharsLimit, P, S, W, Other + charsLen(Str), [Str | Acc]);
                  is_integer(Str) ->
                     buildSmall(Cs, CharsLimit, P, S, W, Other + 1, [Str | Acc]);
                  true ->
                     buildSmall(Cs, CharsLimit, P, S, W, Other, [Str | Acc])
               end
         end;
      _ ->
         if
            is_binary(OneCA) orelse is_list(OneCA) ->
               buildSmall(Cs, CharsLimit, P, S, W, Other + charsLen(OneCA), [OneCA | Acc]);
            is_integer(OneCA) ->
               buildSmall(Cs, CharsLimit, P, S, W, Other + 1, [OneCA | Acc]);
            true ->
               buildSmall(Cs, CharsLimit, P, S, W, Other, [OneCA | Acc])
         end
   end.

ctlSmall($e, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   floatE(Args, Width, Adjust, Precision, PadChar);
ctlSmall($f, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   floatF(Args, Width, Adjust, Precision, PadChar);
ctlSmall($g, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   floatG(Args, Width, Adjust, Precision, PadChar);
ctlSmall($b, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   unPrefixedInt(Args, Width, Adjust, ?base(Precision), PadChar, true);
ctlSmall($B, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   unPrefixedInt(Args, Width, Adjust, ?base(Precision), PadChar, false);
ctlSmall($x, {Args, Prefix}, Width, Adjust, Precision, PadChar, _Encoding) ->
   case is_atom(Prefix) of
      true ->
         prefixedInt(Args, Width, Adjust, ?base(Precision), PadChar, atom_to_binary(Prefix, utf8), true);
      _ ->
         prefixedInt(Args, Width, Adjust, ?base(Precision), PadChar, Prefix, true)

   end;
ctlSmall($X, {Args, Prefix}, Width, Adjust, Precision, PadChar, _Encoding) ->
   case is_atom(Prefix) of
      true ->
         prefixedInt(Args, Width, Adjust, ?base(Precision), PadChar, atom_to_binary(Prefix, utf8), false);
      _ ->
         Base = ?base(Precision),
         prefixedInt(Args, Width, Adjust, Base, PadChar, integer_to_binary(Base), $#, true)
   end;
ctlSmall($+, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   Base = ?base(Precision),
   prefixedInt(Args, Width, Adjust, Base, PadChar, integer_to_binary(Base), $#, true);
ctlSmall($#, Args, Width, Adjust, Precision, PadChar, _Encoding) ->
   Base = ?base(Precision),
   prefixedInt(Args, Width, Adjust, Base, PadChar, integer_to_binary(Base), $#, false);
ctlSmall($c, Args, Width, Adjust, Precision, PadChar, Encoding) ->
   case Encoding of
      unicode ->
         char(Args, Width, Adjust, Precision, PadChar);
      _ ->
         char(Args band 255, Width, Adjust, Precision, PadChar)
   end;
ctlSmall($~, _Args, Width, Adjust, Precision, PadChar, _Encoding) -> char($~, Width, Adjust, Precision, PadChar);
ctlSmall($n, _Args, Width, Adjust, Precision, PadChar, _Encoding) -> newline(Width, Adjust, Precision, PadChar);
ctlSmall($i, _Args, _Width, _Adjust, _Precision, _PadChar, _Encoding) -> ignore;
ctlSmall($s, Args, Width, Adjust, Precision, PadChar, Encoding) when is_atom(Args) ->
   case Encoding of
      latin1 ->
         AtomBinStr = writeAtom(Args, latin1);
      _ ->
         AtomBinStr = writeAtom(Args, uft8)
   end,
   string(AtomBinStr, Width, Adjust, Precision, PadChar, Encoding);
ctlSmall(_C, _Args, _Width, _Adjust, _Precision, _PadChar, _Encoding) -> not_small.

buildLimited([], _, _, _, _, Acc) -> Acc;
buildLimited([OneCA | Cs], NumOfPs, Count, MaxLen, I, Acc) ->
   case OneCA of
      #fmtSpec{ctlChar = CtlChar, args = Args, width = Width, adjust = Adjust, precision = Precision, padChar = PadChar, encoding = Encoding, strings = Strings} ->
         MaxChars = if MaxLen < 0 -> MaxLen; true -> MaxLen div Count end,
         IoListStr = ctlLimited(CtlChar, Args, Width, Adjust, Precision, PadChar, Encoding, Strings, MaxChars, I),
         NewNumOfPs = ?IIF(CtlChar == $p orelse CtlChar == $P, NumOfPs - 1, NumOfPs),
         NewCount = Count - 1,
         NewMaxLen = ?IIF(MaxLen < 0, MaxLen, remainChars(MaxLen, charsLen(IoListStr))),
         if
            NewNumOfPs > 0 ->
               buildLimited(Cs, NewNumOfPs, NewCount, NewMaxLen, I, [IoListStr | Acc]);
            true ->
               buildLimited(Cs, NewNumOfPs, NewCount, NewMaxLen, I, [IoListStr | Acc])
         end;
      _ ->
         buildLimited(Cs, NumOfPs, Count, MaxLen, I + 1, [OneCA | Acc])
   end.

%% (CtlChar, Args, Width, Adjust, Precision, PadChar, Encoding, Strings, MaxChars, I)
ctlLimited($s, Args, Width, Adjust, Precision, PadChar, Encoding, _Strings, CharsLimit, _I) ->
   case Encoding of
      latin1 ->
         BinStr = iolist_to_binary(Args);
      _ ->
         BinStr =
            case catch unicode:characters_to_binary(Args, unicode) of
               Str when is_binary(Str) -> Str;
               _ -> toBinary(Args)
            end
   end,
   TemBinStr = strToChars(BinStr, Width, CharsLimit),
   string(TemBinStr, ?IIF(CharsLimit < 0 orelse Width =:= none, Width, max(3, min(Width, CharsLimit))), Adjust, Precision, PadChar, Encoding);
ctlLimited($w, Args, Width, Adjust, Precision, PadChar, Encoding, _Strings, CharsLimit, _I) ->
   Chars = write(Args, -1, Encoding, CharsLimit),
   term(Chars, Width, Adjust, Precision, PadChar);
ctlLimited($p, Args, Width, _Adjust, _Precision, _PadChar, Encoding, Strings, CharsLimit, _I) ->
   write(Args, -1, ?IIF(Width == none, ?LineCCnt, Width), CharsLimit, Encoding, Strings);
ctlLimited($W, [Args, Depth], Width, Adjust, Precision, PadChar, Encoding, _Strings, CharsLimit, _I) ->
   Chars = write(Args, Depth, Encoding, CharsLimit),
   term(Chars, Width, Adjust, Precision, PadChar);
ctlLimited($P, [Args, Depth], Width, _Adjust, _Precision, _PadChar, Encoding, Strings, CharsLimit, _I) ->
   write(Args, Depth, ?IIF(Width == none, ?LineCCnt, Width), CharsLimit, Encoding, Strings).

term(BinStrOrIoList, Width, Adjust, Precision, PadChar) ->
   if
      Width == none andalso Precision == none ->
         BinStrOrIoList;
      Width == none ->
         StrLen = charsLen(BinStrOrIoList),
         NewPrecision = min(StrLen, Precision),
         if
            StrLen > NewPrecision ->
               adjust(Adjust, makePadChars($*, NewPrecision, <<>>), <<>>);
            true ->
               adjust(Adjust, BinStrOrIoList, makePadChars(PadChar, Precision - StrLen, <<>>))
         end;
      true ->
         StrLen = charsLen(BinStrOrIoList),
         NewPrecision = min(StrLen, case Precision of none -> Width; _ -> min(Precision, Width) end),
         if
            StrLen > NewPrecision ->
               adjust(Adjust, makePadChars($*, NewPrecision, <<>>), makePadChars(PadChar, Width - NewPrecision, <<>>));
            true ->
               adjust(Adjust, BinStrOrIoList, makePadChars(PadChar, Width - StrLen, <<>>))
         end
   end.

floatE(Float, Width, Adjust, Precision, PadChar) ->
   NewPrecision = ?IIF(Precision == none, 6, Precision),

   case Width of
      none ->
         float_to_binary(Float, [{scientific, NewPrecision}]);
      _ ->
         term(float_to_binary(Float, [{scientific, NewPrecision}]), Width, Adjust, Width, PadChar)
   end.

floatF(Float, Width, Adjust, Precision, PadChar) ->
   NewPrecision = ?IIF(Precision == none, 6, Precision),

   case Width of
      none ->
         float_to_binary(Float, [{decimals, NewPrecision}]);
      _ ->
         term(float_to_binary(Float, [{decimals, NewPrecision}]), Width, Adjust, Width, PadChar)
   end.

floatG(Float, Width, Adjust, Precision, PadChar) ->
   case Float > -10000.0 andalso Float < 10000.0 of
      true ->
         floatF(Float, Width, Adjust, Precision, PadChar);
      _ ->
         floatE(Float, Width, Adjust, Precision, PadChar)
   end.

floatG(Float) ->
   float_to_binary(Float, [{decimals, 6}]).

strToChars(BinStr, Width, CharsLimit) ->
   ByteSize = byte_size(BinStr),
   if
      Width == none ->
         case CharsLimit < 0 orelse CharsLimit >= ByteSize of
            true ->
               BinStr;
            _ ->
               <<(part(BinStr, 0, CharsLimit))/binary, "...">>
         end;
      CharsLimit < 0 orelse CharsLimit >= Width ->
         BinStr;
      true ->
         <<(part(BinStr, 0, CharsLimit))/binary, "...">>
   end.

string(Str, Width, Adjust, Precision, PadChar, Encoding) ->
   if
      Width == none andalso Precision == none ->
         Str;
      Precision == none ->
         strField(Str, Width, Adjust, charsLen(Str), PadChar, Encoding);
      Width == none ->
         strField(Str, Precision, left, charsLen(Str), PadChar, Encoding);
      true ->
         StrLen = charsLen(Str),
         if
            Width > Precision ->
               if
                  StrLen > Precision ->
                     adjust(Adjust, flatTrunc(Str, Precision, Encoding), makePadChars(PadChar, Width - Precision, <<>>));
                  StrLen < Precision ->
                     adjust(Adjust, [Str | makePadChars(PadChar, Precision - StrLen, <<>>)], makePadChars(PadChar, Width - Precision, <<>>));
                  true -> % N == P
                     adjust(Adjust, Str, makePadChars(PadChar, Width - Precision, <<>>))
               end;
            true -> % F == P
               strField(Str, Width, Adjust, StrLen, PadChar, Encoding)
         end
   end.

strField(Str, Width, Adjust, StrLen, PadChar, Encoding) when StrLen > Width ->
   if
      StrLen > Width ->
         flatTrunc(Str, Width, Encoding);
      StrLen < Width ->
         adjust(Adjust, Str, makePadChars(PadChar, Width - StrLen, <<>>));
      true ->
         Str
   end.

flatTrunc(List, Width, _Encoding) ->
   part(iolist_to_binary(List), 0, Width).

makePadChars(PadChar, Cnt, BinStr) ->
   case Cnt > 0 of
      true ->
         makePadChars(PadChar, Cnt - 1, <<BinStr/binary, PadChar:8>>);
      _ ->
         BinStr
   end.

adjust(left, Data, Pad) -> [Data, Pad];
adjust(right, Data, Pad) -> [Pad, Data].

unPrefixedInt(Int, Width, Adjust, Base, PadChar, Lowercase) ->
   case Lowercase of
      true ->
         term(toLowerStr(integer_to_binary(Int, Base)), Width, Adjust, none, PadChar);
      _ ->
         term(integer_to_binary(Int, Base), Width, Adjust, none, PadChar)
   end.

prefixedInt(Int, Width, Adjust, Base, PadChar, Prefix, Lowercase) ->
   case Int < 0 of
      true ->
         case Lowercase of
            true ->
               term(<<"-", (toBinary(Prefix))/binary, (toLowerStr(integer_to_binary(-Int, Base)))/binary>>, Width, Adjust, none, PadChar);
            _ ->
               term(<<"-", (toBinary(Prefix))/binary, (integer_to_binary(-Int, Base))/binary>>, Width, Adjust, none, PadChar)
         end;
      _ ->
         case Lowercase of
            true ->
               term(<<(toBinary(Prefix))/binary, (toLowerStr(integer_to_binary(Int, Base)))/binary>>, Width, Adjust, none, PadChar);
            _ ->
               term(<<(toBinary(Prefix))/binary, (integer_to_binary(Int, Base))/binary>>, Width, Adjust, none, PadChar)
         end
   end.

prefixedInt(Int, Width, Adjust, Base, PadChar, Prefix, Prefix2, Lowercase) ->
   case Int < 0 of
      true ->
         case Lowercase of
            true ->
               term(<<"-", (toBinary(Prefix))/binary, Prefix2:8, (toLowerStr(integer_to_binary(-Int, Base)))/binary>>, Width, Adjust, none, PadChar);
            _ ->
               term(<<"-", (toBinary(Prefix))/binary, Prefix2:8, (integer_to_binary(-Int, Base))/binary>>, Width, Adjust, none, PadChar)
         end;
      _ ->
         case Lowercase of
            true ->
               term(<<(toBinary(Prefix))/binary, Prefix2:8, (toLowerStr(integer_to_binary(Int, Base)))/binary>>, Width, Adjust, none, PadChar);
            _ ->
               term(<<(toBinary(Prefix))/binary, Prefix2:8, (integer_to_binary(Int, Base))/binary>>, Width, Adjust, none, PadChar)
         end
   end.

char(Char, Width, Adjust, Precision, PadChar) ->
   if
      Width == none andalso Precision == none ->
         Char;
      Precision == none ->
         makePadChars(Char, Width, <<>>);
      Width == none ->
         makePadChars(Char, Precision, <<>>);
      true ->
         adjust(Adjust, makePadChars(Char, Precision, <<>>), makePadChars(PadChar, Width - Precision, <<>>))
   end.

newline(none, _Adjust, _Precision, _PadChar) -> <<"\n">>;
newline(Width, Adjust, _Precision, _PadChar) ->
   case Adjust of
      right ->
         makePadChars($\n, Width, <<>>);
      _ ->
         <<"\n">>
   end.

remainChars(T, E) ->
   if
      T < 0 ->
         T;
      T >= E ->
         T - E;
      true ->
         0
   end.
%% ********************************************** eFmtFormat end   *****************************************************
%% ********************************************** utils start **********************************************************
toLowerStr(BinStr) ->
   <<begin
        case C >= $A andalso C =< $Z of
           true ->
              <<(C + 32)>>;
           _ ->
              <<C>>
        end
     end || <<C:8>> <= BinStr
   >>.

toUpperStr(BinStr) ->
   <<begin
        case C >= $a andalso C =< $z of
           true ->
              <<(C - 32)>>;
           _ ->
              <<C>>
        end
     end || <<C:8>> <= BinStr
   >>.

-spec charsLen(chars()) -> non_neg_integer().
charsLen(S) ->
   try
      iolist_size(S)
   catch
      _:_ ->
         string:length(S)
   end.

getOpt(Key, TupleList, Default) ->
   case keyfind(Key, 1, TupleList) of
      false ->
         Default;
      ValueTuple ->
         element(2, ValueTuple)
   end.

toBinary(Value) when is_integer(Value) -> integer_to_binary(Value);
toBinary(Value) when is_list(Value) -> list_to_binary(Value);
toBinary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 6}, compact]);
toBinary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
toBinary(Value) when is_binary(Value) -> Value;
toBinary(Value) -> term_to_binary(Value).

visualList(L, Encoding) ->
   ?IIF(Encoding == latin1, visualLatin1List(L), visualUnicodeList(L, Encoding)).

visualBin(Bin, Encoding) ->
   ?IIF(Encoding == latin1, visualLatin1Bin(Bin), visualUtf8Bin(Bin, io:printable_range())).

visualLatin1List([]) -> true;
visualLatin1List([C | Cs]) -> ?IIF(visualLatin1Char(C), visualLatin1List(Cs), false);
visualLatin1List(_) -> false.

visualUnicodeList([], _) -> true;
visualUnicodeList([C | Cs], Encoding) -> ?IIF(visualUtf8Char(C, Encoding), visualUnicodeList(Cs, Encoding), false);
visualUnicodeList(_, _) -> false.

visualLatin1Bin(<<>>) -> true;
visualLatin1Bin(<<C:8, Left/binary>>) -> ?IIF(visualLatin1Char(C), visualLatin1Bin(Left), false);
visualLatin1Bin(_) -> false.

visualUtf8Bin(<<>>, _) -> true;
visualUtf8Bin(<<C/utf8, Left/binary>>, Range) -> ?IIF(visualUtf8Char(C, Range), visualUtf8Bin(Left, Range), false);
visualUtf8Bin(_, _) -> false.

visualLatin1Char($\n) -> true;
visualLatin1Char($\r) -> true;
visualLatin1Char($\t) -> true;
visualLatin1Char($\v) -> true;
visualLatin1Char($\b) -> true;
visualLatin1Char($\f) -> true;
visualLatin1Char($\e) -> true;
visualLatin1Char(C) -> C >= $\040 andalso C =< $\176 orelse C >= $\240 andalso C =< $\377.

visualUtf8Char($\n, _) -> true;
visualUtf8Char($\r, _) -> true;
visualUtf8Char($\t, _) -> true;
visualUtf8Char($\v, _) -> true;
visualUtf8Char($\b, _) -> true;
visualUtf8Char($\f, _) -> true;
visualUtf8Char($\e, _) -> true;
visualUtf8Char(C, _Encoding) ->
   C >= $\s andalso C =< $~ orelse C >= 16#A0 andalso C < 16#D800 orelse C > 16#DFFF andalso C < 16#FFFE orelse C > 16#FFFF andalso C =< 16#10FFFF.
%% case Encoding of
%%    latin1 ->
%%       C >= $\s andalso C =< $~ orelse C >= 16#A0 andalso C =< 16#FF;
%%    _ ->
%%       C >= $\s andalso C =< $~ orelse C >= 16#A0 andalso C < 16#D800 orelse C > 16#DFFF andalso C < 16#FFFE orelse C > 16#FFFF andalso C =< 16#10FFFF
%% end.
%% ********************************************** utils end   **********************************************************