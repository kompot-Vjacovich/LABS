# Лабараторные работы по СМПР
# Иванча Николай
## Метрические алгоритмы
<center>
<table>
  <tbody>
    <tr>
      <th>Метод</th>
      <th>Параметры</th>
      <th>Точность</th>
    </tr>
    <tr>
      <td>kNN</td>
      <td>k=6</td>
      <td>0.0333</td>
    </tr>
    <tr>
      <td>kwNN</td>
      <td>k=19, q=0.8</td>
      <td>0.04</td>
    </tr>  
    <tr>
      <td>Парзеновское окно, Прямоугольное</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td>Парзеновское окно, Треугольное</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td>Парзеновское окно, Квартическое</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td>Парзеновское окно, Епанечниково</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td>Парзеновское окно, Гаусовское</td>
      <td>h=0.1</td>
      <td>0.04</td>
    </tr>
  </tbody>
   </table>
### Алгоритм ближайших соседей kNN
**Алгоритм ближайших соседей kNN** - это метрический алгоритм классификации, для реализации которого необходимо последовательно выполнить следующие операции:
* Посчитать расстояние до каждого из объектов обучающей выборки
* Отсортировать выборку в порядке близости к классифицируемому объекту
* Выбрать k  первых элементов(k ближайших соседей)
* Классом классифицируемого объекта будет являться тот клас, который чаще всего встречается среди k ближайших соседей

|Карта классификации kNN|
|-------------------|
|![Карта классификации алгоритма kNN](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/neighbors/Classification_kNN.png)|

В качестве обучающей выборке был взят набор "Ирисы Фишера", а в качестве функции расстояния было использованно Евклидово расстояние

``` R
distance <- function(u, v)
{
	sqrt(sum((u - v)^2))
}
```
Для более точного процесса классификации необходимо правильно подобрать параметр k. Воспользуемся критерием скользящего контроля по отдельным объектам - **LOO**(leave-one-out). Суть данного критерия в разделении обучающей выборки на подвыборку для тренировки и один элемент для контроля. Преимущество LOO в том, что каждый объект ровно один раз участвует в контроле, а длина обучающих подвыборок лишь на единицу меньше длины полной выборки.

![equation](http://latex.codecogs.com/gif.latex?LOO%28k%2C%20X%5El%29%20%3D%20%5Csum_%7Bi%3D1%7D%5E%7Bl%7D%20%5Balgo%28x_i%3B%20X%5El%20%5Cbackslash%20%5C%7B%20x_i%5C%7D%2C%20k%29%20%5Cneq%20y_i%5D%5Crightarrow%20%5Cmin_k)

Если алгоритм классификации при данном k неправильно определяет класс классифицируемого объекта, то оценка для этого k ухудшается. Оптимальным считается тот k, оценка которого наилучшая.

Оценка скользящего контроля LOO алгоритма k-ближайших соседей для данного набора показала, что классификация более точна при k=6.

График оценки скользящего контроля, а также карта классификации выглядят следующим образом:

LOO(k) алгоритма kNN|Карта классификации kNN, при k = 6
:------------------:|:--------------------------------:
![LOO для kNN](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/neighbors/Loo_for_kNN.png)|![Карта классификации алгоритма kNN, k=6](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/neighbors/Classification_kNN.png)

### Алгоритм классификации "kwNN" k-взвешенных соседей:
Недостаток kNN в том, что максимум может достигаться сразу на нескольких классах. В задачах с двумя классами этого можно избежать, если взять нечётное k. Более общая тактика, которая годится и для случая многих классов — ввести строго убывающую последовательность вещественных весов ![w_i](https://latex.codecogs.com/gif.latex?w_i)
, задающих вклад i-го соседа в классификацию.  
Все объекты выборки сортируются по удаленности от классифицируемого объекта. Выбираются k ближайших соседей.
Классифицируемый объект относим к тому классу, суммарный вес которого больше.
В реализованном методе выбрана евклидова метрика.  
В качестве выборки был взят набор "Ирисы Фишера".  
В качестве последовательности весов взята нелинейно убывающую последовательность - геометрическая прогрессия: ![w_i = q^i](https://latex.codecogs.com/gif.latex?w_i%20%3D%20q%5Ei), где знаменатель прогрессии ![q ∈ (0, 1)](https://latex.codecogs.com/gif.latex?q%20%5Cin%20%280%2C%201%29%24) является параметром алгоритма. Его можно подбирать по критерию LOO, аналогично числу соседей k. 

LOO(k) алгоритма kwNN|Карта классификации kwNN, при k = 6, q = 0.8
:------------------:|:--------------------------------:
![LOO для kwNN](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/neighbors/LOO_for_kwNN.png)|![Карта классификации алгоритма kwNN, k=6,q=0.8](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/neighbors/Classification_kwNN.png)

На следующем примере наглядно продемонстрированно превосходство алгоритма классификации kwNN над алгоритмом kNN:

|Сравнение kNN и kwNN|
|-------------------|
|![Сравнение kNN и kwNN](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/neighbors/knnORkwnn.png)|

Данный пример показывает преимущество алгоритма kwNN перед kNN, при k большем, чем размер выборки. В даном случае вес каждого элемента выборки играет значимую роль.

### Алгоритм классификации "Парзеновское окно":

В данном алгоритме весовая функция ![w_i] определяется как функция от расстояния между классифицируемым объектом u и его соседями ![x(u_i), i = 1, ..., l], а не от ранга соседа i, как было kwNN.

### Реализация на R
```R
#Parsen Windows
parsen <- function(dots, x, h=c(0.35), core=cG){
  dots <- sortDist(dots, x)
  
  class <- list()
  
  classes <- as.list(rep(0, 3))
  names(classes) = levels(dots$Species)
  
  for (i in seq(length(h))) {
    for (j in seq(length(dots[[1]]))) {
      yi <- dots[j,]
      classes[[yi$Species]] <- classes[[yi$Species]] + core(yi$Distance / h[i])
    }
    
    if(max(unlist(classes)) > 0) {
      class[i] <- names(sort(unlist(classes), decreasing = TRUE))[1]
    }
    else class[i] <- "unknown"
  }
  
  res <- unlist(class)
}
```
**Подберём оптимальное h, используя метод скользящего контроля, и построим карты классификации для всех ядер:**

|Прямоугольное ядро| Карта классификации и LOO
:------------------:|:--------------------------------:
|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/LOO_parsen_Rect.png)|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/Classification_parsen_Rect.png)|

|Треугольное ядро| Карта классификации и LOO
:------------------:|:--------------------------------:
![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/LOO_parsen_Trian.png)|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/Classification_parsen_Trian.png)

|Квартическое ядро| Карта классификации и LOO
:------------------:|:--------------------------------:
![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/LOO_parzen_Quart.png)|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/Classification_parsen_Quart.png)

|Епанечниково ядро| Карта классификации и LOO
:------------------:|:--------------------------------:
![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/LOO_parsen_Epan.png)|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/Classification_parsen_Epan.png)

|Гаусовское ядро| Карта классификации и LOO
:------------------:|:--------------------------------:
![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/LOO_parsen_Gaus.png)|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/parsen/Classification_parsen_Gaus.png)
