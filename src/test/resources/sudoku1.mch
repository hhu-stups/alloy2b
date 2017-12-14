/*@ generated */
MACHINE alloytranslation
SETS
    Row_; Column_; Number_; Board_
CONSTANTS
    Row1_, Row2_, Row3_, Row4_, Row5_, Row6_, Row7_, Row8_, Row9_, Col1_, Col2_, Col3_, Col4_, Col5_, Col6_, Col7_, Col8_, Col9_, One_, Two_, Three_, Four_, Five_, Six_, Seven_, Eight_, Nine_, array_
PROPERTIES
    {Row1_} <: Row_ &
    {Row2_} <: Row_ &
    {Row3_} <: Row_ &
    {Row4_} <: Row_ &
    {Row5_} <: Row_ &
    {Row6_} <: Row_ &
    {Row7_} <: Row_ &
    {Row8_} <: Row_ &
    {Row9_} <: Row_ &
    {Col1_} <: Column_ &
    {Col2_} <: Column_ &
    {Col3_} <: Column_ &
    {Col4_} <: Column_ &
    {Col5_} <: Column_ &
    {Col6_} <: Column_ &
    {Col7_} <: Column_ &
    {Col8_} <: Column_ &
    {Col9_} <: Column_ &
    {One_} <: Number_ &
    {Two_} <: Number_ &
    {Three_} <: Number_ &
    {Four_} <: Number_ &
    {Five_} <: Number_ &
    {Six_} <: Number_ &
    {Seven_} <: Number_ &
    {Eight_} <: Number_ &
    {Nine_} <: Number_ &
    array_[Board_](Row1_)(Col5_) = {Five_} & array_[Board_](Row1_)(Col6_) = {Two_} & array_[Board_](Row1_)(Col8_) = {Four_} & array_[Board_](Row2_)(Col1_) = {One_} & array_[Board_](Row2_)(Col3_) = {Five_} & array_[Board_](Row2_)(Col4_) = {Three_} & array_[Board_](Row3_)(Col2_) = {Six_} & array_[Board_](Row3_)(Col9_) = {Five_} & array_[Board_](Row4_)(Col3_) = {Four_} & array_[Board_](Row4_)(Col6_) = {Seven_} & array_[Board_](Row4_)(Col8_) = {Nine_} & array_[Board_](Row5_)(Col2_) = {Eight_} & array_[Board_](Row5_)(Col4_) = {Two_} & array_[Board_](Row5_)(Col6_) = {Nine_} & array_[Board_](Row5_)(Col8_) = {Five_} & array_[Board_](Row6_)(Col2_) = {Nine_} & array_[Board_](Row6_)(Col4_) = {Six_} & array_[Board_](Row6_)(Col7_) = {Three_} & array_[Board_](Row7_)(Col1_) = {Six_} & array_[Board_](Row7_)(Col8_) = {Two_} & array_[Board_](Row8_)(Col6_) = {Three_} & array_[Board_](Row8_)(Col7_) = {Nine_} & array_[Board_](Row8_)(Col9_) = {One_} & array_[Board_](Row9_)(Col2_) = {Seven_} & array_[Board_](Row9_)(Col4_) = {Eight_} & array_[Board_](Row9_)(Col5_) = {Two_} &
    Row1_ /= Row2_ &
    Row1_ /= Row3_ &
    Row1_ /= Row4_ &
    Row1_ /= Row5_ &
    Row1_ /= Row6_ &
    Row1_ /= Row7_ &
    Row1_ /= Row8_ &
    Row1_ /= Row9_ &
    Row2_ /= Row3_ &
    Row2_ /= Row4_ &
    Row2_ /= Row5_ &
    Row2_ /= Row6_ &
    Row2_ /= Row7_ &
    Row2_ /= Row8_ &
    Row2_ /= Row9_ &
    Row3_ /= Row4_ &
    Row3_ /= Row5_ &
    Row3_ /= Row6_ &
    Row3_ /= Row7_ &
    Row3_ /= Row8_ &
    Row3_ /= Row9_ &
    Row4_ /= Row5_ &
    Row4_ /= Row6_ &
    Row4_ /= Row7_ &
    Row4_ /= Row8_ &
    Row4_ /= Row9_ &
    Row5_ /= Row6_ &
    Row5_ /= Row7_ &
    Row5_ /= Row8_ &
    Row5_ /= Row9_ &
    Row6_ /= Row7_ &
    Row6_ /= Row8_ &
    Row6_ /= Row9_ &
    Row7_ /= Row8_ &
    Row7_ /= Row9_ &
    Row8_ /= Row9_ &
    Col1_ /= Col2_ &
    Col1_ /= Col3_ &
    Col1_ /= Col4_ &
    Col1_ /= Col5_ &
    Col1_ /= Col6_ &
    Col1_ /= Col7_ &
    Col1_ /= Col8_ &
    Col1_ /= Col9_ &
    Col2_ /= Col3_ &
    Col2_ /= Col4_ &
    Col2_ /= Col5_ &
    Col2_ /= Col6_ &
    Col2_ /= Col7_ &
    Col2_ /= Col8_ &
    Col2_ /= Col9_ &
    Col3_ /= Col4_ &
    Col3_ /= Col5_ &
    Col3_ /= Col6_ &
    Col3_ /= Col7_ &
    Col3_ /= Col8_ &
    Col3_ /= Col9_ &
    Col4_ /= Col5_ &
    Col4_ /= Col6_ &
    Col4_ /= Col7_ &
    Col4_ /= Col8_ &
    Col4_ /= Col9_ &
    Col5_ /= Col6_ &
    Col5_ /= Col7_ &
    Col5_ /= Col8_ &
    Col5_ /= Col9_ &
    Col6_ /= Col7_ &
    Col6_ /= Col8_ &
    Col6_ /= Col9_ &
    Col7_ /= Col8_ &
    Col7_ /= Col9_ &
    Col8_ /= Col9_ &
    One_ /= Two_ &
    One_ /= Three_ &
    One_ /= Four_ &
    One_ /= Five_ &
    One_ /= Six_ &
    One_ /= Seven_ &
    One_ /= Eight_ &
    One_ /= Nine_ &
    Two_ /= Three_ &
    Two_ /= Four_ &
    Two_ /= Five_ &
    Two_ /= Six_ &
    Two_ /= Seven_ &
    Two_ /= Eight_ &
    Two_ /= Nine_ &
    Three_ /= Four_ &
    Three_ /= Five_ &
    Three_ /= Six_ &
    Three_ /= Seven_ &
    Three_ /= Eight_ &
    Three_ /= Nine_ &
    Four_ /= Five_ &
    Four_ /= Six_ &
    Four_ /= Seven_ &
    Four_ /= Eight_ &
    Four_ /= Nine_ &
    Five_ /= Six_ &
    Five_ /= Seven_ &
    Five_ /= Eight_ &
    Five_ /= Nine_ &
    Six_ /= Seven_ &
    Six_ /= Eight_ &
    Six_ /= Nine_ &
    Seven_ /= Eight_ &
    Seven_ /= Nine_ &
    Eight_ /= Nine_ &
    {Row1_} \/ {Row2_} \/ {Row3_} \/ {Row4_} \/ {Row5_} \/ {Row6_} \/ {Row7_} \/ {Row8_} \/ {Row9_} = Row_ &
    {Col1_} \/ {Col2_} \/ {Col3_} \/ {Col4_} \/ {Col5_} \/ {Col6_} \/ {Col7_} \/ {Col8_} \/ {Col9_} = Column_ &
    {One_} \/ {Two_} \/ {Three_} \/ {Four_} \/ {Five_} \/ {Six_} \/ {Seven_} \/ {Eight_} \/ {Nine_} = Number_ &
    /* from signature declaration */ !(this_).({this_} <: Board_ => !(r_).({r_} <: Row_ => array_[{this_}](r_)(Column_) = Number_) & !(c_).({c_} <: Column_ => array_[{this_}](Row_)(c_) = Number_) & array_[{this_}]({Row1_} \/ {Row2_} \/ {Row3_})({Col1_} \/ {Col2_} \/ {Col3_}) = Number_ & array_[{this_}]({Row1_} \/ {Row2_} \/ {Row3_})({Col4_} \/ {Col5_} \/ {Col6_}) = Number_ & array_[{this_}]({Row1_} \/ {Row2_} \/ {Row3_})({Col7_} \/ {Col8_} \/ {Col9_}) = Number_ & array_[{this_}]({Row4_} \/ {Row5_} \/ {Row6_})({Col1_} \/ {Col2_} \/ {Col3_}) = Number_ & array_[{this_}]({Row4_} \/ {Row5_} \/ {Row6_})({Col4_} \/ {Col5_} \/ {Col6_}) = Number_ & array_[{this_}]({Row4_} \/ {Row5_} \/ {Row6_})({Col7_} \/ {Col8_} \/ {Col9_}) = Number_ & array_[{this_}]({Row7_} \/ {Row8_} \/ {Row9_})({Col1_} \/ {Col2_} \/ {Col3_}) = Number_ & array_[{this_}]({Row7_} \/ {Row8_} \/ {Row9_})({Col4_} \/ {Col5_} \/ {Col6_}) = Number_ & array_[{this_}]({Row7_} \/ {Row8_} \/ {Row9_})({Col7_} \/ {Col8_} \/ {Col9_}) = Number_) &
    array_ : Board_ --> Row_ * Column_ --> Number_
OPERATIONS
    run_2 = PRE card(Board_) >= 1 THEN skip END;
    check_2 = PRE not(card(Board_) = 1) THEN skip END
END
