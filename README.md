# Лабараторные работы по СМПР
# Иванча Николай

<ol><a name="content"></a>
  <li>
    <a href="#Metric">Метрические алгоритмы</a>
    <ul>
      <li><a href="#kNN">Алгоритм ближайших соседей kNN</a></li>
      <li><a href="#kwNN">Алгоритм классификации kwNN</a></li>
      <li><a href="#parsen">Парзеновское окно</a></li>
      <li><a href="#potential">Потенциальные функции</a></li>
    </ul>
  </li>
  <li>
    <a href="#Bayes">Байесовские классификаторы</a>
    <ul>
      <li><a href="#levels">Линии уровня нормального распределения</a></li>
      <li><a href="#naive">Наивный байесовский классификатор</a></li>
    </ul>
  </li>
</ol>

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
	<td><a href="#kNN">kNN</a></td>
      <td>k=6</td>
      <td>0.0333</td>
    </tr>
    <tr>
      <td><a href="#kwNN">kwNN</a></td>
      <td>k=19, q=0.8</td>
      <td>0.04</td>
    </tr>  
    <tr>
      <td><a href="#parsen">Парзеновское окно</a>, Прямоугольное</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#parsen">Парзеновское окно</a>, Треугольное</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#parsen">Парзеновское окно</a>, Квартическое</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#parsen">Парзеновское окно</a>, Епанечниково</td>
      <td>h=0.35</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#parsen">Парзеновское окно</a>, Гаусовское</td>
      <td>h=0.1</td>
      <td>0.04</td>
    </tr>
    <tr>
      <td><a href="#potential">Потенциальные функции</a>, Гаусовское</td>
      <td>h=1(50),h=0.5(100) </td>
      <td>Переменная</td>
    </tr>
  </tbody>
   </table>
	
	
### <a name="kNN"></a>Алгоритм ближайших соседей kNN
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

<a href="#content">Вернуться к содержанию</a>

### <a name="kwNN"></a>Алгоритм классификации kwNN k-взвешенных соседей:
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

<a href="#content">Вернуться к содержанию</a>

### <a name="parsen"></a>Алгоритм классификации "Парзеновское окно":

В данном алгоритме весовая функция ![w_i](https://latex.codecogs.com/gif.latex?w_i) определяется как функция от расстояния между классифицируемым объектом u и его соседями ![x(u_i), i = 1, ..., l](http://latex.codecogs.com/gif.latex?x%28u_i%29%2C%5C%20i%20%3D%201%2C%5Cldots%2C%20l), а не от ранга соседа i, как было kwNN.

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

<a href="#content">Вернуться к содержанию</a>

###  <a name="potential"></a>Метод потенциальных функций

В реализуемом методе используется фиксированная ширина окна. Для первых 50 объектов (class=setosa) h=1, так как объекты данного класса достаточно удалены от объектов других. Для остальных объектов h=0.5.

Изачально потенциалы заполняются нулями. Далее, пока количество ошибок классификации не достигнет нужного предела, выбираем случайно точку x из выборки. Если для нее классификация выполняется неверно, увеличиваем потенциал на 1 и пересчитываем общее количество ошибок.

Ниже представленны карта потенциалов и карта классификации для Гаусовского ядра. Радиус круга - величина потенциала.

|Карта потенциалов| Карта классификации
:------------------:|:--------------------------------:
![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/potfunc/potential_circles.png)|![](https://github.com/kompot-Vjacovich/LABS/blob/master/MetricAlgo/potfunc/Classification_potential.png)

С помощью данного метода мы можем классифицировать объекты с максимальной возможной точностью, которую мы можем задать вручную при подборе параметров.

<a href="#content">Вернуться к содержанию</a>

## <a name="Bayes"></a>Байесовские классификаторы
### <a name="levels"></a>Линии уровня нормального распределения

N-мерным нормальным распределением будет называться распределение с плотностью 
![](https://user-images.githubusercontent.com/44859059/50251637-90a34200-03f5-11e9-8c17-2fc8cf8fcb9b.png)
с математическим ожиданием (μ) и матрицей ковариации (σ).

Релизация с помощью <a href="https://kompot-vjacovich.shinyapps.io/levels/">shiny</a>

### Реализация на R
```R
draw_lines <- function(mu, mtx, title) { #n-мерное гауссовское распределение
  a11 <- mtx[1,1]
  a12 <- mtx[1,2]
  a21 <- mtx[2,1]
  a22 <- mtx[2,2]
  
  det = det(mtx)
  
  A <- a11/det
  B <- a22/det
  C <- (-a12-a21)/det
  D <- (-2*a11*mu[1] + mu[2]*(a12+a21))/det
  E <- (-2*a22*mu[2] + mu[1]*(a12+a21))/det
  F <- (a11*mu[1]^2 + a22*mu[2]^2 - mu[1]*mu[2]*(a12+a21))/det
  
  N <- function(x,y) (1/(sqrt((2*pi)^2*det(mtx))))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  
  x <- seq(-4, 4, 8/200)
  y <- seq(-4, 4, 8/200)
  z <- outer(x, y, N)
  
  par(pty="s")
  contour(x, y, z, main=title)
  
}
```

|Математическое ожидание μ  | Матрица ковариаций σ | Визуализация
:------------------:|:----------------------------:| :------------------:
(0,0)| matrix(1,0,0,1) |![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/levels/lines1.png)
(0,0)| matrix(2,1,1,2) |![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/levels/lines2.png)
(0,0)| matrix(3,0,0,1) |![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/levels/lines3.png)
(0,0)| matrix(1,0,0,3) |![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/levels/lines4.png)

<a href="#content">Вернуться к содержанию</a>

### <a name="naive"></a>Наивный байесовский классификатор

<b>Наивный байесовский классификатор</b> – это алгоритм классификации, который принимает одно допущение: Каждый признак классифицируемых данных рассматривается независимо от других признаков класса. Метод называется наивным т.к. предполагается, что все признаки набора данных независимы, что встречается крайне редко.

Обычно он используется, как эталон при сравнении различных алгоритмов классификации.
Решающее правило принимает вид: 

![](https://camo.githubusercontent.com/8bc9dd137568ab3e91170c9160834d144265d638/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f61253238782532392533446172672532302535436d61785f25374279253543696e253230592537442532382535436c6e2532382535436c616d6264615f25374279253744505f792532392b25354373756d5f2537426a253344312537442535452537426e2537442535436c6e253238705f253742796a25374425323825354378695f6a253239253239253239)

Реализация на R
```R
naive_bayes <- function(xl, len1, len2, P, lyambda) {
  p <- function(ksi, mu, sigma) (1/(sigma*sqrt(2*pi)))*exp(-(ksi-mu)^2 / (2*sigma^2))
  
  calc_mu <- function(xl) sum(xl) / length(xl)
  
  calc_sigma <- function(xl, mu) sum((xl-mu)^2)/(length(xl)-1)
  
  classification <- function(x, classes, mu, sigma, Py, lyambda) {
    classSum <- rep(0, length(classes))
    names(classSum) <- classes
    
    for (i in 1:length(classes)) {
      tmpSum <- 0
      
      for (j in 1:length(x)) {
        tmP <- p(x[j], mu[i,j], sigma[i,j])
        tmpSum <- tmpSum + log(tmP)
      }
      classSum[i] <- tmpSum + log(lyambda[i]*Py[i])
    }
    
    return(names(which.max(classSum)))
  }
  
  classify_all <- function(classes, mu, sigma, Py, lyambda) {
    classifiedObj <- c()
    
    for(i in seq(min(mu[, 1]) - 5, max(mu[, 1]) + 5, 0.1)) {
      for(j in seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, 0.1)) {
        classifiedObj <- rbind(classifiedObj, c(i, j, classification(c(i, j), classes, mu, sigma, Py, lyambda)))
      }
    }
    
    return(classifiedObj)
  }
  
  Py <- P
  len <- len1 + len2
  first_x <- xl[1:len1,1]
  first_y <- xl[1:len1,2]
  second_x <- xl[(len1+1):len,1]
  second_y <- xl[(len1+1):len,2]
  
  mu <- rbind(c(calc_mu(first_x), calc_mu(first_y)), c(calc_mu(second_x), calc_mu(second_y)))
  
  sigma <- rbind(c(calc_sigma(first_x, mu[1,1]), calc_sigma(first_y, mu[1,2])), c(calc_sigma(second_x, mu[2,1]), calc_sigma(second_y, mu[2,2])))
  
  classes <- unique(xl[,ncol(xl)])
  
  classified <- classify_all(classes, mu, sigma, Py, lyambda)
}
```

Визуализация работы алгоритма при помощи <a href="https://kompot-vjacovich.shinyapps.io/naive/">shiny</a>

![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/Naive/Classification_Naive.png)

<a href="#content">Вернуться к содержанию</a>
