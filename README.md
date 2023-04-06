# Soya 🌱
#### 💚 Suitable for Vegans! 💚

You like Python but also consider yourself a *{bracket-head}*? Try some Soya, seriously.  (psst... there are some semicolons as well).

Soya has bean (pun intended) created for JPP course @ MIM UW as dynamically typed language with static binding, a cross between Python and C++. 

### Contains:

- **Fun and intuitive declarations**

  It makes Soya suitable for children

  ```PYTHON
  x = 3; # it's a new int!
  y = str; # oldschool way if you have to!
  
  x = 6; # it's assignment!
  ```


- **Default function arguments**

  Soya is cool with that by default.
  
  ```python
  func multiply(a = int, b = 2) {
      return a * b;
  }
  
  print(multiply(4)); # 8
  ```


- **Tuples :snake:** 

  *yes, I see you all pythonistas!* :point_up:

  ```python
  tup = t(1, t("hello", "world"));
  
  t(_, msg) = tup; # unpacking is vegan friendly
  
  print(msg); # 'hello', 'world'
  
  msg[0] = "yup, tuples are mutable";
  ```

- **Pass by reference/variable**

  so... all that python stuff was a lie?

  ```python
  # note the ref keyword
  func modify_some_stuff(ref stuff = int, value = 3) {
      stuff = value;
  }
  
  a = 5;
  modify_some_stuff(a);
  print(a); # 3
  ```

- **Handling None and no arguments passed** :ghost:

  Because sometimes you just don't have energy for another argument :roll_eyes:

   ```python
   uninitialized = int;
   
   print(uninitialized); # None
   print(isna uninitialized); # True
   
   func weird_func(a = int, b = int) {
       return t(a, b);
   }
   
   print(weird_func()); # (None, None)
   ```

- **Static type List**:

  it was just too hard to implement a dynamic list tbh

  ```python
  a = [1, 2, 3]; # 3 element list of ints
  b = [str]; # empty list of strings
  ```

- **Soya doesn't float!**

  ```python
  print(5 / 3); # 1
  ```



### Make it run! :gear:

```bash

```



### Detailed ingredients :notebook:

```
01 (trzy typy)
02 (literały, arytmetyka, porównania)
03 (zmienne, przypisanie)
04 (print)
05 (while, if)
06 (funkcje lub procedury, rekurencja)
07 (przez zmienną / przez wartość)
08 (zmienne read-only w pętli for)
09 (przesłanianie i statyczne wiązanie)
10 (obsługa błędów wykonania)
11 (funkcje zwracające wartość)

13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
15 (2) (krotki z przypisaniem)
16 (1) (break, continue)

EXTRA:
17 (domyślne wartości argumentów w funkcjach)
18 (domyślanie się typu zmiennej i obsługa None)
19 (operator _ ignorujący wynik przypisania)

Oczekiwana liczba punktów
27 + EXTRA = 30
```



