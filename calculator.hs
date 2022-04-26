type Operator = Double -> Double -> Double -- binarniy operator
type Entry = (String, Operator)-- cortage string i operator
type Register = [Entry]--spisok

operatorRegister :: Register -- slovar, kotoriy sopostovlyaet stringu sootvetstvuiushuyu operaciyu
operatorRegister = [
        (")", error "ERROR: Invalid parenthesis placement!"),
        ("(", error "ERROR: Invalid parenthesis placement!"),
        ("+", (+)),
        ("-", (-)),
        ("*", (*)),
        ("/", (/))
    ] 

numbers :: String -> Bool -> [String] --vsya eta funkciya string perevodit v massiv stringov kazshdiy iz kotorih yavlyaetsya operatorom
numbers [] True  = [""]--pusto+true=spisok is pustoy stroki
numbers [] False = []--pusto+false=pusto
numbers (x:xs) prev--x-head xs-tail prev-previos lexing storki na chars
        | x == ' ' = if prev then "" :       rest else       rest -- ternarny operator cotori zamenyaet probeli na pustie mesta
        | x == '+' = if prev then "" : "+" : rest else "+" : rest
        | x == '-' = if prev then "" : "-" : rest else "-" : rest
        | x == '*' = if prev then "" : "*" : rest else "*" : rest
        | x == '/' = if prev then "" : "/" : rest else "/" : rest
        | x == '(' = if prev then "" : "(" : rest else "(" : rest
        | x == ')' = if prev then "" : ")" : rest else ")" : rest
        | otherwise = (x : head nrest) : tail nrest
    where
        rest  = numbers xs False
        nrest = numbers xs True 

isNumber :: String -> Bool
isNumber str =
    case reads str :: [(Double, String)] of
        [(_, "")] -> True
        _         -> False -- proveryaet yavlyaetsya li stroka chislom

mySpan :: (String -> Bool) -> [String] -> ([String], [String]) -- (String -> Bool) - eto finciya perevodiashaya string v bool (proverka usloviya)
mySpan _ [] = ([], []) -- esli net suffiksa vozvrashaet pustie spisku (konstructor po umolchaniu)
mySpan predic lst = if predic $ last lst -- primenyaem predikat k poslednemu elementu
    then (left, right ++ [last lst]) -- ++ konkotenaciya spiskov (k pravomu spisku v konec pribavlyaem posledniy element iz lst kotoriy udovletvoryaet predicatu)
    else (lst, []) -- leviy element eto stroka ne udovletvoryaushaya predicatu
    where(left, right) = mySpan predic $ init lst  -- where - (telo funkcii) -- ishet pervuyu sleva posledovatelnost stringov udovletvoryaushaya predicatu
-- delaet razrez po poslednemu vhozdeniu stringa


parenthesis :: Register -> [String] -> [String] -- prinimaet slovar operaciy i neparsenoe virazshenie || SYUDA PRIHODIT STROKA BEZ ")"
parenthesis [] _   = error "ERROR: Invalid parenthesis placement!" -- esli skobka ne nashlas, to oshibka
parenthesis ((operator, function):rest) unparsed = 
    case mySpan (/= operator) unparsed of -- /= == !=
        ([], _) -> parenthesis rest unparsed -- esli ne nashli ")", to ishem "("
        (left, right) -- esli nashli, chto iskali
            | operator == ")" -> parenthesis operatorRegister (parenthesis operatorRegister (init left) ++ right) -- esli nashli ")", to nam teper nado nayti dve "(", poetomu dva raza vizivaem parenthesis 
            | operator == "(" -> init left ++ [show (evaluate operatorRegister right)] -- show - preobrazuet double v string || esli nashli "(", to ")" - zakrita, a znachit, to chto vnutri mozshno poschitat, toest: evaluate right, i potom vozvrashaem poschitannoe vnutri skobok virazshenie
            | otherwise -> error "ERROR: Invalid parenthesis placement!"


calculate :: String -> Double
calculate str = evaluate operatorRegister $ numbers str False

evaluate :: Register -> [String] -> Double
evaluate _ [] = error "ERROR: Invalid syntax!"
evaluate _ [left, right] -- esli dva stringa, to + i - unarnie operatori, togda right - eto chislo, a ostalnoe ne pravilno
    | left == "+" =   evaluate operatorRegister [right]
    | left == "-" = - evaluate operatorRegister [right]
    | left == "(" || left == ")" || right == "(" || right == ")" = error "ERROR: Invalid parenthesis placement!"
    | left == "*" || left == "/"                 = error "ERROR: Invalid use of the operator!"
    | otherwise = error "ERROR: Invalid value!"

evaluate _ [number] -- esli stroka chislo, to vozvrashaet eto chislo 
    | isNumber number = read number
    | otherwise = error "ERROR: Invalid value!"

evaluate ((operator, function):rest) unparsed =
    case mySpan (/= operator) unparsed of
        ([], _) -> evaluate rest unparsed -- esli operator ne nayden, to parsim sleduyushiy operator
        (left, right) -- operator nayden
            | operator == ")" -> evaluate operatorRegister ((parenthesis (take 2 operatorRegister) $ init left) ++ right)  -- zapuskaem analizator skobochnoho virasheniya ot stroki bez ")"  
            | otherwise -> function (evaluate operatorRegister $ init left) (evaluate operatorRegister right) -- esli poka ne vstretili ")", to nasholsya arefmeticheskiy operator, i vizivaetsya ocenka ot ego pravoy i levoy chasti, a na ih rezultati srabativaet operator

main :: IO() -- input / output
main = interact ( unlines . map (show . calculate) . lines)
