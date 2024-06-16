## SUPPORT2

To repozytorium zawiera pliki potrzebne do realizacji projektu z przedmiotów Zaawansowane metody uczenia maszynowego oraz Wdrażanie modeli uczenia maszynowego.

W ramach przedmiotu Zaawansowane metody uczenia maszynowego, przeprowadzono analizę danych z repozytorium UCI (https://archive.ics.uci.edu/dataset/880/support2) dotyczących pacjentów przebywających w stanie krytycznym w ośrodkach medycznych w Stanach Zjednoczonych. Na tej podsatwie zbudowano zaawansowane modele uczenia maszynowego przewidujących(?) trzy cechy (zadanie *multioutput*). Raport jest dostępny w formacie HTML jako plik `support.html`.

W ramach przedmiotu Wdrażanie modeli uczenia maszynowego przygotowano aplikację w systemie Shiny, umożliwiającą m. in. import analizowanych danych, ich eksplorację oraz ewaluację modelu. Aplikacja jest dostępna jako plik `shiny.R`.

### Cel badania

Celem badania było zbudowanie zaawansowanego modelu uczenia maszynowego prognozującego stopień niepełnosprawności funkcjonalnej pacjenta oraz prawdopodobieństwo śmierci (ogólnie), jak i w szpitalu. Będzie to pomocne głównie lekarzom i pielęgnioarkom oraz osobom opiekującym się chorymi we wcześniejszym podejmowaniu odpowiednich decyzji co do procesu leczenia i planowania w celu zmniejszenia częstotliwości mechanicznego, bolesnego i przedłużającego się procesu umierania.

### Opis aplikacji

Przed uruchomieniem należy pobrać dane `support2.csv` dostępne w tym repozytorium oraz wszystkie pliki o rozszerzeniu `.rds` - są to gotowe architektury modeli oraz dane, na których w aplikacji można dokonywać ich ewaluacji. Wszystkie pliki powinny znajdować sie w jednym folderze.

Aplikację można uruchomić wpisaując w wyszukiwarce następującą formułę: http://127.0.0.1:5150/ lub otwierając plik `shiny.R` w programie RStudio oraz nacisnąć przycisk *Run App* znajdujący się w prawym górnym rogu programu.

*rys. instr1.png*

#### Instrukcja użytkowania

Po uruchomieniu aplikacji, powinna ona wyglądać tak:

*rys. str_tytulowa.png*

##### Wstęp

Pierwsza zakładka to strona tytułowa. Stanowi ona wstęp do całego badania oraz aplikacji. Jest tu możliwość importu (odpowiednich) danych - jeśli nie wczyta się ich, nie będzie możliwości użytkowania aplikacji do celów, do jakich została stworzona.

*rys. instr2.png*

Po wczytaniu odpowiedniego pliku z danymi (dedykowanymi do tej aplikacji), aplikacja automatycznie umożliwia ich podgląd. Użytkownik może zdefiniować liczbę wyświetlanych wierszy, przeglądać kolejne strony wczytanego zbioru, czy sortować dane według wybranej przez siebie zmiennej.

*rys. podglad_danych.png*

##### EDA

Drugi panel umożliwia eksploracyjną analizę wczytanych danych. Składa się on z dwóch głównych części: pierwsza to krótka charakterystyka wczytanych danych. Druga składa się z trzech rozwijanych kart, które dotyczą różnych aspektów EDA. W "Zmiennych numerycznych" w pierwszej zakładce przedstawiono histogram, wybranej wcześniej z rozwijanego menu, zmiennej, a w drugiej zakładce widoczne są statystyki opisowe tej zmiennej.

Druga karta dotyczy zmiennych kategorycznych - w pierwszej zakładce przedstawiany jest histogram wybranej przez użytkownika zmiennej, a w drugiej zakładce przedstawiony jest jej dokładny rozkład liczebnościowy.

Trzecia karta umożliwia przedstawienie wykresu ramka-wąsy dla wybranej zmiennej, a w drugiej zakładce, po zdefiniowaniu drugiej zmiennej z menu rozwijanego, wyświetlana jest wartość korelacji pomiędzy wybranymi zmiennymi.

*rys. eda2.png*

*rys. eda3.png*

##### Wizualizacje

Ten panel umożliwia wizualizacje wykresów gęstości wybranych zmiennych z podanych grupach. Składa się ona z czterech rozwijanych kart. Pierwsza dotyczy zmiennej `dzclass`, druga zmiennej `ca`, trzecia `diabetes`, a czwarta `dementia`. W każdej z karcie, po jej rozwinięciu, widnieje także wyjaśnienie znaczenia danej zmiennej w tym zbiorze danych.

*rys. eda4.png*

*rys. eda5.png*

##### Modele i wyniki

W zakładce "Modele i wyniki", po wybraniu jednego z czterech modeli z listy rozwijanej, rysowana jest jego historia uczenia na zbiorze treningowym wczytanych przez użytkowanika danych, oraz wyświetlane są metryki tego modelu na danych testowyc, a pod spodem także trzy macierze klasyfikacji - każda dotyczy jednej z przewidywanych przez modele zmiennych w tym zbiorze.

*rys. modele1.png*

*rys. modele2.png*


#### Informacje dodatkowe

Aplikacja zawiera odpowiednią obsługę błedów.

*rys. blad1.png*

*rys. blad2.png*

Całe badanie - zarówno raport, jak i aplikacja - zostały opracowane za pomocą języka programowania R w programie RStudio.