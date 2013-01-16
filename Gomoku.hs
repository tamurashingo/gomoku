-- Haskell練習
-- 五目並べ

module Gomoku where

{---------------------------------------------------------------------
 - 基礎部分
 ---------------------------------------------------------------------}

-- 石の定義。
data Ishi = Blank
          | Black
          | White
     deriving (Eq, Show)


-- 次の順番。
-- 黒→白、白→黒。
next :: Ishi -> Ishi
next ishi
     | ishi == Black = White
     | ishi == White = Black
     | otherwise     = Blank


-- ゲーム盤の定義。
data Board board = Board [[board]] deriving (Show)


-- 置ける場所がない？
gomokuIsOver :: Board Ishi -> Bool
gomokuIsOver (Board board) =
    let
        check []     = True
        check (x:xs) = if checkRow x
                       then check xs
                       else False
        checkRow []     = True
        checkRow (x:xs) = if x /= Blank
                          then checkRow xs
                          else False
    in
      check board

-- 幅
getWidth :: Board Ishi -> Int
getWidth (Board board) = length (head board)

-- 高さ
getHeight :: Board Ishi -> Int
getHeight (Board board) = length board


-- 初期のゲーム盤を作成する。
makeEmptyBoard :: Int -> Int -> Board Ishi
makeEmptyBoard width height = Board (replicate height (replicate width Blank))


-- 指定した場所の石を取得する。
getIshi :: Board Ishi -> Int -> Int -> Ishi
getIshi (Board board) x y = board !! y !! x

-- 置けるかチェック。
isBlank :: Board Ishi -> Int -> Int -> Bool
isBlank board x y = getIshi board x y == Blank

-- 石を置く。
putIshi :: Board Ishi -> Int -> Int -> Ishi -> Board Ishi
putIshi (Board board) x y ishi =
    let
        row = take x (board !! y) ++ [ishi] ++ drop (x+1) (board !! y)
    in
      Board (take y board ++ [row] ++ drop (y+1) board)


-- 置いた場所を通る行、列、左上から右下、右上から左下の石のリストを生成する。
getList :: Board Ishi -> Int -> Int -> [[Ishi]]
getList (Board board) x y =
    let
        -- 行
        getRow = board !! y
        -- 列
        getCol = map (!! x) board

        -- ゲーム盤のサイズを取得しておく。
        width = getWidth (Board board)
        height = getHeight (Board board)
        -- 左上、右上の座標を取得
        startUL | x < y     = (0, y-x)
                | otherwise = (x-y, 0)
        startUR | width-1-x < y = (width-1, y-(width-1-x))
                | otherwise     = (x+y, 0)
        -- 左上(UpLeft)から右下(DownRight)の石をリスト化
        getULtoDR x y lst | x < width && y < height = getULtoDR (x+1) (y+1) (lst ++ [(getIshi (Board board) x y)])
                          | otherwise               = lst
        -- 右上(UpRight)から左下(DownLeft)の石をリスト化
        getURtoDL x y lst | 0 <= x && y < height = getURtoDL (x-1) (y+1) (lst ++ [(getIshi (Board board) x y)])
                          | otherwise            = lst
    in
      -- リストのリスト
      [getRow,
       getCol,
       getULtoDR (fst startUL) (snd startUL) [],
       getURtoDL (fst startUR) (snd startUR) []]

-- 上で生成したリストの中に、指定した石が５個並んでいたらTrueが返る。
checkFive :: [[Ishi]] -> Ishi -> Bool
checkFive ishiList ishi | ishiList == [] = False
                        | otherwise =
                            let
                                checkLoop lst cnt | cnt == 5         = True
                                                  | lst == []        = checkFive (tail ishiList) ishi
                                                  | head lst == ishi = checkLoop (tail lst) (cnt+1)
                                                  | otherwise        = checkLoop (tail lst) 0
                            in
                              checkLoop (head ishiList) 0



{---------------------------------------------------------------------
 - IO部分
 ---------------------------------------------------------------------}

-- ゲーム盤の表示
showBoard :: Board Ishi -> IO ()
showBoard (Board (x:xs)) =
    let
        showAll [] cnt       = return ()
        showAll (x1:xs1) cnt = do showRow x1 cnt
                                  showAll xs1 (1+cnt)
        showRow [] cnt                    = do putStr "..."
                                               print cnt
        showRow (x1:xs1) cnt | x1 == Blank = do putStr "__"
                                                showRow xs1 cnt
                             | x1 == Black = do putStr "O "
                                                showRow xs1 cnt
                             | x1 == White = do putStr "X "
                                                showRow xs1 cnt
    in
      do
        putStrLn "0 1 2 3 4 5 6 7 "
        putStrLn "----------------"
        showAll (x:xs) 0


-- 置く場所を標準入力から取得する。
input :: IO (Int, Int)
input =
    do
      putStr "input row[0-7]:"
      row <- getLine
      putStr "input col[0-7]:"
      col <- getLine
      return ((read col :: Int), (read row :: Int))

-- 置ける場所が入力されるまで待つ。
gomokuInputResult :: Board Ishi -> IO (Int, Int)
gomokuInputResult board =
    do
      te <- input
      let 
          x = fst te
          y = snd te
      if isBlank board x y
      then return te
      else do
        putStrLn "bad location!!"
        gomokuInputResult board


-- メインループ
gomokuLoop :: Board Ishi -> Ishi -> IO ()
gomokuLoop board ishi | gomokuIsOver board = putStrLn "draw"
                      | otherwise          =
                          do
                            showBoard board
                            putTurn ishi
                            te <- gomokuInputResult board
                            let
                                x = fst te
                                y = snd te
                            newBoard <- return (putIshi board x y ishi)
                            if (checkFive (getList newBoard x y) ishi)
                            then do
                              showBoard newBoard
                              putStrLn "win"
                            else gomokuLoop newBoard (next ishi)
                          where
                            putTurn Black = putStrLn "++BLACK++"
                            putTurn White = putStrLn "--WHITE--"

-- メイン処理
main :: IO ()
main = gomokuLoop (makeEmptyBoard 8 8) Black


