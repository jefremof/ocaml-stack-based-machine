# ocaml-stack-based-machine
OCaml implementation of the simple stack-based machine

## Формат инструкций

Инструкции упакованы по шесть в 32-битное слово: шесть 5-битных слотов (S0–S5) плюс
два неиспользуемых старших бита. Именно это распаковывает `word_to_batch` в
[lib/machine.ml](lib/machine.ml). Слова-данные (адреса переходов, литералы) занимают
слово целиком.

![Формат инструкционного слова Gullwing](docs/instruction-format.svg)

Диаграмма перерисована по мотивам Fig. 6.3 из работы LaForest (см. «Источники и атрибуция»).

## Источники и атрибуция

Проект основан на архитектуре стековой машины **Gullwing**, описанной в дипломной работе:

> Charles Eric LaForest. *Second-Generation Stack Computer Architecture*.
> Thesis, Bachelor of Independent Studies, Independent Studies Program,
> University of Waterloo, Canada, апрель 2007.
> <https://fpgacpu.ca/stack/Second-Generation_Stack_Computer_Architecture.pdf>

**Как в этом репозитории используются материалы работы:**

- Пояснения и формулировки изложены своими словами со ссылкой на источник.
- Диаграммы и таблицы, если они приводятся, перерисованы и пересоставлены заново, с
  пометкой «адаптировано из [LaForest 2007]». Дословное воспроизведение рисунков и
  таблиц из работы не выполняется.
- Короткие цитаты даются в кавычках с указанием источника и, по возможности, страницы.
