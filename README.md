# fmi-fp-haskell-2018

Упражнения по Функционално Програмиране с Haskell, специалност ИС, 2018г.


#### Как да си настроим средата?

1. Ако нямаме инсталиран Haskell сваляме Haskell Platform Full от https://www.haskell.org/platform/ и го инсталираме.

2. Сваляме _SciTE_ архива от moodle курса. Разархивираме цялото съдържание на архива в папка по избор (ако отворим само `.exe` файла директно от архива няма да се заредят другите конфигурационни файлове и ще излизат проблеми при компилация).


#### Как да пишем в нов файл ?

1. Създаваме нов файл и изрично изписваме разширението `.hs` след името му. Избиране на `.hs` от падащото меню не работи ако не изпишем ръчно разширението.

2. Дефинираме функцията която искаме да използваме.

3. Дефинираме `main` функция в следния вид за да тестваме горедефинираната функция.

4. За да run-нем в _SciTE_, натискаме **F5**.

Пример за файл `max.hs`:
```haskell
max :: Integer → Integer → Integer
max a b = if a > b
    then a
    else b

main = do
    print (max 1 2)
    -- ...
    print (max 5 8)
```


#### Как да използваме файловете със задачи?

1. Заместваме `undefined` с нужната дефиниция. Не е нужно да пипаме нищо друго по файловете или да копираме функциите в други файлове.

2. В `main` функцията се извикват няколко функции които изкарват на екрана резултатите от няколко тестови случая.


#### Как да ползваме нещо различно от _SciTE_?

Пишем файловете по гореописания начин с предпочитания от нас текстов редактор. Отваряме стандартния command line interface на операционната система (terminal – Linux), (cmd – Windows) и изписваме една от следните комбинации от команди:

  - `runghc program.hs`

  - `ghc program.hs` и после стартираме новосъздадения бинарен изпълним файл `program.exe` (или `.o` вместо `.exe` за Linux).
