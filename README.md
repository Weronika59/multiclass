# SUPPORT2

To repozytorium zawiera pliki potrzebne do realizacji projektu z przedmiotów Zaawansowane metody uczenia maszynowego oraz Wdrażanie modeli uczenia maszynowego.

W ramach przedmiotu Zaawansowane metody uczenia maszynowego, przeprowadzono analizę danych z repozytorium UCI (https://archive.ics.uci.edu/dataset/880/support2) dotyczących pacjentów przebywających w stanie krytycznym w ośrodkach medycznych w Stanach Zjednoczonych. Na tej podsatwie zbudowano zaawansowane modele uczenia maszynowego przewidujących(?) trzy cechy (zadanie *multioutput*). Raport jest dostępny w formacie HTML jako plik `support.html`.

W ramach przedmiotu Wdrażanie modeli uczenia maszynowego przygotowano aplikację w systemie Shiny, umożliwiającą m. in. import analizowanych danych, ich eksplorację oraz ewaluację modelu. Aplikacja jest dostępna jako plik `shiny.R`.

## Cel badania

Celem badania było zbudowanie zaawansowanego modelu uczenia maszynowego prognozującego stopień niepełnosprawności funkcjonalnej pacjenta oraz prawdopodobieństwo śmierci (ogólnie), jak i w szpitalu. Będzie to pomocne głównie lekarzom i pielęgnioarkom oraz osobom opiekującym się chorymi we wcześniejszym podejmowaniu odpowiednich decyzji co do procesu leczenia i planowania w celu zmniejszenia częstotliwości mechanicznego, bolesnego i przedłużającego się procesu umierania.

## Opis aplikacji

Przed uruchomieniem należy pobrać dane `support2.csv` dostępne w tym repozytorium oraz wszystkie pliki o rozszerzeniu `.rds` - są to gotowe architektury modeli oraz dane, na których w aplikacji można dokonywać ich ewaluacji. Wszystkie pliki powinny znajdować sie w jednym folderze.

Aplikację można uruchomić wpisaując w wyszukiwarce następującą formułę: http://127.0.0.1:5150/ lub otwierając plik `shiny.R` w programie RStudio oraz nacisnąć przycisk *Run App* znajdujący się w prawym górnym rogu programu.

![instr1](https://github.com/Weronika59/multiclass/assets/75950630/3caa23cb-68b2-4fcd-8256-c04e1cee5823)


### Instrukcja użytkowania

Po uruchomieniu aplikacji, powinna ona wyglądać tak:

![str_tytulowa](https://github.com/Weronika59/multiclass/assets/75950630/8c9a1f92-33be-4534-b31d-9b285611189a)


#### Wstęp

Pierwsza zakładka to strona tytułowa. Stanowi ona wstęp do całego badania oraz aplikacji. Jest tu możliwość importu (odpowiednich) danych - jeśli nie wczyta się ich, nie będzie możliwości użytkowania aplikacji do celów, do jakich została stworzona.

![instr2](https://github.com/Weronika59/multiclass/assets/75950630/6093ce6f-f4fa-4368-ba9d-686a58e04fdd)

Po wczytaniu odpowiedniego pliku z danymi (dedykowanymi do tej aplikacji), aplikacja automatycznie umożliwia ich podgląd. Użytkownik może zdefiniować liczbę wyświetlanych wierszy, przeglądać kolejne strony wczytanego zbioru, czy sortować dane według wybranej przez siebie zmiennej.

![podglad_danych](https://github.com/Weronika59/multiclass/assets/75950630/3024a64c-090c-4d75-8350-27cb2b762075)


#### EDA

Drugi panel umożliwia eksploracyjną analizę wczytanych danych. Składa się on z dwóch głównych części: pierwsza to krótka charakterystyka wczytanych danych. Druga składa się z trzech rozwijanych kart, które dotyczą różnych aspektów EDA. W "Zmiennych numerycznych" w pierwszej zakładce przedstawiono histogram, wybranej wcześniej z rozwijanego menu, zmiennej, a w drugiej zakładce widoczne są statystyki opisowe tej zmiennej.

Druga karta dotyczy zmiennych kategorycznych - w pierwszej zakładce przedstawiany jest histogram wybranej przez użytkownika zmiennej, a w drugiej zakładce przedstawiony jest jej dokładny rozkład liczebnościowy.

Trzecia karta umożliwia przedstawienie wykresu ramka-wąsy dla wybranej zmiennej, a w drugiej zakładce, po zdefiniowaniu drugiej zmiennej z menu rozwijanego, wyświetlana jest wartość korelacji pomiędzy wybranymi zmiennymi.

![eda2](https://github.com/Weronika59/multiclass/assets/75950630/9065d89f-1e68-4003-a04d-5eab2d5923f9)

![eda3](https://github.com/Weronika59/multiclass/assets/75950630/89d13b2a-7441-41db-874a-dd766bfe1335)


#### Wizualizacje

Ten panel umożliwia wizualizacje wykresów gęstości wybranych zmiennych z podanych grupach. Składa się ona z czterech rozwijanych kart. Pierwsza dotyczy zmiennej `dzclass`, druga zmiennej `ca`, trzecia `diabetes`, a czwarta `dementia`. W każdej z karcie, po jej rozwinięciu, widnieje także wyjaśnienie znaczenia danej zmiennej w tym zbiorze danych.

![eda4](https://github.com/Weronika59/multiclass/assets/75950630/7b872258-eb7f-4b45-8acf-b1a2c9334f14)

![eda5](https://github.com/Weronika59/multiclass/assets/75950630/0afeb3a5-6edf-4ecc-9e82-c075fa35a7c7)


#### Modele i wyniki

W zakładce "Modele i wyniki", po wybraniu jednego z czterech modeli z listy rozwijanej, rysowana jest jego historia uczenia na zbiorze treningowym wczytanych przez użytkowanika danych, oraz wyświetlane są metryki tego modelu na danych testowyc, a pod spodem także trzy macierze klasyfikacji - każda dotyczy jednej z przewidywanych przez modele zmiennych w tym zbiorze.

![modele1](https://github.com/Weronika59/multiclass/assets/75950630/513592f2-4217-49ef-ad46-55a2b9094de2)

![modele2](https://github.com/Weronika59/multiclass/assets/75950630/d034b327-8d34-4c5c-82af-7dd8732cc6cb)


### Informacje dodatkowe

Aplikacja zawiera odpowiednią obsługę błedów.

![blad1](https://github.com/Weronika59/multiclass/assets/75950630/050fdd9d-81cb-432a-8f1c-52f9c23f9b29)

![blad2](https://github.com/Weronika59/multiclass/assets/75950630/4c6c6ba5-89b9-41a7-a7b2-27a20f3ac513)

Całe badanie - zarówno raport, jak i aplikacja - zostały opracowane za pomocą języka programowania R w programie RStudio.
