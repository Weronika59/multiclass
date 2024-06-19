## SUPPORT2

To repozytorium zawiera pliki potrzebne do realizacji projektu z przedmiotów Zaawansowane metody uczenia maszynowego oraz Wdrażanie modeli uczenia maszynowego.

W ramach przedmiotu Zaawansowane metody uczenia maszynowego, przeprowadzono analizę danych z repozytorium UCI (https://archive.ics.uci.edu/dataset/880/support2) dotyczących pacjentów przebywających w stanie krytycznym w ośrodkach medycznych w Stanach Zjednoczonych. Na tej podsatwie zbudowano zaawansowane modele uczenia maszynowego przewidujących(?) trzy cechy (zadanie *multioutput*). Raport jest dostępny w formacie HTML jako plik `support.html`.

W ramach przedmiotu Wdrażanie modeli uczenia maszynowego przygotowano aplikację w systemie Shiny, umożliwiającą m. in. import analizowanych danych, ich eksplorację oraz ewaluację modelu. Aplikacja jest dostępna jako plik `shiny.R`.

### Cel badania

Celem badania było zbudowanie zaawansowanego modelu uczenia maszynowego prognozującego stopień niepełnosprawności funkcjonalnej pacjenta oraz prawdopodobieństwo śmierci (ogólnie), jak i w szpitalu. Będzie to pomocne głównie lekarzom i pielęgnioarkom oraz osobom opiekującym się chorymi we wcześniejszym podejmowaniu odpowiednich decyzji co do procesu leczenia i planowania w celu zmniejszenia częstotliwości mechanicznego, bolesnego i przedłużającego się procesu umierania.

### Opis aplikacji

Przed uruchomieniem należy pobrać dane `support2.csv` dostępne w tym repozytorium oraz wszystkie pliki o rozszerzeniu `.rds` i `.keras` - są to gotowe architektury modeli oraz dane, na których w aplikacji można dokonywać ich ewaluacji. Wszystkie pliki powinny znajdować sie w jednym folderze.

Aplikację należy uruchomić otwierając plik `shiny.R` w programie RStudio i naciskając przycisk *Run App* znajdujący się w prawym górnym rogu programu.

![instr1](https://github.com/Weronika59/multiclass/assets/75950630/090edcfd-b70d-4871-8bce-3f7d24d70583)

Aplikację, po uruchomieniu w programie RStusio, można także otworzyć w przeglądarce.

#### Instrukcja użytkowania

Po uruchomieniu aplikacji, powinna ona wyglądać tak:

![str_tytulowa](https://github.com/Weronika59/multiclass/assets/75950630/30cb4b08-2150-4f90-9de8-4a4205f40431)


##### Wstęp

Pierwsza zakładka to strona tytułowa. Stanowi ona wstęp do całego badania oraz aplikacji. Jest tu możliwość importu (odpowiednich) danych - jeśli nie wczyta się ich, nie będzie możliwości użytkowania aplikacji do celów, do jakich została stworzona.

![instr2](https://github.com/Weronika59/multiclass/assets/75950630/edf3a037-3c6e-41ca-8234-938e4b4fdbbc)


Po wczytaniu odpowiedniego pliku z danymi (dedykowanymi do tej aplikacji), aplikacja automatycznie umożliwia ich podgląd. Użytkownik może zdefiniować liczbę wyświetlanych wierszy, przeglądać kolejne strony wczytanego zbioru, czy sortować dane według wybranej przez siebie zmiennej.

![podglad_danych](https://github.com/Weronika59/multiclass/assets/75950630/eeee3cd3-546c-45fc-b9a4-bfe5db57849c)


##### EDA

Drugi panel umożliwia eksploracyjną analizę wczytanych danych. Składa się on z dwóch głównych części: pierwsza to krótka charakterystyka wczytanych danych. Druga składa się z trzech rozwijanych kart, które dotyczą różnych aspektów EDA. W "Zmiennych numerycznych" w pierwszej zakładce przedstawiono histogram, wybranej wcześniej z rozwijanego menu, zmiennej, a w drugiej zakładce widoczne są statystyki opisowe tej zmiennej.

![eda1](https://github.com/Weronika59/multiclass/assets/75950630/5148ea3a-8691-4e35-9a6d-1483c49f4a35)

Druga karta dotyczy zmiennych kategorycznych - w pierwszej zakładce przedstawiany jest histogram wybranej przez użytkownika zmiennej, a w drugiej zakładce przedstawiony jest jej dokładny rozkład liczebnościowy.

Trzecia karta umożliwia przedstawienie wykresu ramka-wąsy dla wybranej zmiennej, a w drugiej zakładce, po zdefiniowaniu drugiej zmiennej z menu rozwijanego, wyświetlana jest wartość korelacji pomiędzy wybranymi zmiennymi.

![eda2](https://github.com/Weronika59/multiclass/assets/75950630/ea46601b-990f-4a5f-839c-64ba7f51f272)

![eda3](https://github.com/Weronika59/multiclass/assets/75950630/ab96efdb-9ad8-4cc2-8ad2-81c3d29f4cb4)


##### Wizualizacje

Ten panel umożliwia wizualizacje wykresów gęstości wybranych zmiennych z podanych grupach. Składa się ona z czterech rozwijanych kart. Pierwsza dotyczy zmiennej `dzclass`, druga zmiennej `ca`, trzecia `diabetes`, a czwarta `dementia`. W każdej z karcie, po jej rozwinięciu, widnieje także wyjaśnienie znaczenia danej zmiennej w tym zbiorze danych.

![eda4](https://github.com/Weronika59/multiclass/assets/75950630/0a903102-434f-4325-9a5f-7d1a4de42281)

![eda5](https://github.com/Weronika59/multiclass/assets/75950630/0111b6bd-2552-45c9-8502-b183c0b566e6)


##### Modele i wyniki

W zakładce "Modele i wyniki", po wybraniu jednego z czterech modeli z listy rozwijanej, rysowana jest jego historia uczenia na zbiorze treningowym wczytanych przez użytkowanika danych, oraz wyświetlane są metryki tego modelu na danych testowych, a pod spodem także trzy macierze klasyfikacji - każda dotyczy jednej z przewidywanych przez modele zmiennych w tym zbiorze.

![modele1](https://github.com/Weronika59/multiclass/assets/75950630/47a79c9d-fafe-40a6-ba50-2620caf62657)

![modele2](https://github.com/Weronika59/multiclass/assets/75950630/7ec751cf-f341-4169-bb9e-10fb858ac15a)


#### Informacje dodatkowe

Aplikacja zawiera odpowiednią obsługę błedów.

![blad1](https://github.com/Weronika59/multiclass/assets/75950630/b0526b31-4d12-40a6-98a0-00efdb8dba35)

![blad2](https://github.com/Weronika59/multiclass/assets/75950630/ba766452-68ec-4987-bf9b-d2ed36fef988)


Całe badanie - zarówno raport, jak i aplikacja - zostały opracowane za pomocą języka programowania R w programie RStudio.

**Pamiętaj, aby w pliku `shiny.R` zmienić ścieżki do potrzebnych plików, aby aplikacja była w pełni funkcjonalna!**
