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
      <li><a href="#plug-in">Plug-in</a></li>
      <li><a href="#LDF">Линейный дискриминант фишера</a></li>
    </ul>
  </li>
  <li>
    <a href="#Linear">Линейные алгоритмы классификации</a>
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
calc_mu <- function(xl) sum(xl) / length(xl)

calc_sigma <- function(xl, mu) sum((xl-mu)^2)/(length(xl)-1)

naive_bayes <- function(xl, len1, len2, P, lyambda, mu, sigma) {
  p <- function(ksi, mu, sigma) (1/(sigma*sqrt(2*pi)))*exp(-(ksi-mu)^2 / (2*sigma^2))
  
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
  
  draw_plot <- function(xl, classifiedObj) {
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    points(classifiedObj[, 1:(n-1)], pch = 21, col = colors[classifiedObj[, n]])
  }
  
  Py <- P
  len <- len1 + len2
  
  classes <- unique(xl[,ncol(xl)])
  
  classified <- classify_all(classes, mu, sigma, Py, lyambda)
  draw_plot(xl, classified)
}

```

Визуализация работы алгоритма при помощи <a href="https://kompot-vjacovich.shinyapps.io/naive/">shiny</a>

![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/Naive/Classification_Naive.png)

<a href="#content">Вернуться к содержанию</a>

### <a name="naive"></a>Plug-in

В качестве модели восстанавливаемой плотности в <b>Подстановочном алгоритме(Plug-in)</b> рассматривается многомерная нормальная плотность: 

![](https://camo.githubusercontent.com/8e7cf0a285068cff21acc2a6d67cfaa81d85d184/68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f4e253238782532432532302535436d752532432532302535435369676d612532392532302533442532302535436672616325374231253744253742253543737172742537422532383225354370692532392535456e2537432535435369676d61253743253744253744657870253238253543667261632537422d3125374425374232253744253238782532302d2532302535436d75253239253545542532302535435369676d612535452537422d31253744253238782532302d2532302535436d7525323925323925324325323078253230253543657073696c6f6e2532302535436d6174686262253742522537442535452537426e253744)

где ![](https://latex.codecogs.com/gif.latex?%5Cmu%20%5Cepsilon%20%5Cmathbb%7BR%7D%5E%7Bn%7D) -- математическое ожидание (центр), а ![](https://latex.codecogs.com/gif.latex?%5CSigma%20%5Cepsilon%20%5Cmathbb%7BR%7D%5E%7Bn%5Ctimes%20n%7D) - ковариационная матрица 

Алгоритм заключается в восстановлении параметров нормального распределения ![](https://latex.codecogs.com/gif.latex?%5Cmu_y), ![](https://latex.codecogs.com/gif.latex?%5CSigma_y) для каждого класса ![](https://latex.codecogs.com/gif.latex?y%20%5Cepsilon%20Y) и подстановке их в формулу оптимального байесовского классификатора. Предполагается, что ковариационные матрицы классов не равны.

Оценка параметров нормального распределения производится на основе *принципа максимума правдоподобия*:

![](https://latex.codecogs.com/gif.latex?%5Cmu_y%20%3D%20%5Cfrac%7B1%7D%7Bl_y%7D%5Csum_%7Bx_i%3Ay_i%20%3D%20y%7D%20x_i) ,

![](https://latex.codecogs.com/gif.latex?%5CSigma_y%20%3D%20%5Cfrac%7B1%7D%7Bl_y%20-%201%7D%5Csum_%7Bx_i%3Ay_i%20%3D%20y%7D%28x_i%20-%20%5Cmu_y%29%28x_i%20-%20%5Cmu_y%29%5ET).

Разделяющая поверхность между двумя классами *s* и *t* задаётся следующим образом:

![](https://latex.codecogs.com/gif.latex?%5Clambda_sP_s%5Crho_s%28x%29%20%3D%20%5Clambda_tP_t%5Crho_t%28x%29)

Логарифмируя плотности каждого класса и подставив в выражение с разностью логарифмов, можем получть коэффициенты разделяющей кривой

Реализация на R
```R
calc_mu <- function(xl) sum(xl) / length(xl)

calc_sigma <- function(xl, mu) {
  sum <- 0
  for (i in 1:nrow(xl)) {
    xi <- matrix(c(xl[i,1], xl[i,2]), 1, 2)
    sum <- sum + t(xi - mu) %*% (xi - mu)
  }
  sum / (nrow(xl)-1)
}

plug_in <- function(xl, len1, len2, Py, mu, sigma, l1, l2) {
  
  discriminant <- function(mu, sigma, Py) {
    sigma1 <- sigma[1:2,]
    sigma2 <- sigma[3:4,]
    
    inverse_s1 <- solve(sigma1)
    inverse_s2 <- solve(sigma2)
    
    #xn = x^n
    x2 <- inverse_s1[1,1] - inverse_s2[1,1]
    y2 <- inverse_s1[2,2] - inverse_s2[2,2]
    xy <- 2 * inverse_s1[1,2] - 2 * inverse_s2[1,2]
    
    x1 <- 2 * inverse_s2[1,2] * mu[2,2] - 2 * inverse_s1[1,2] * mu[1,2] - 2 * inverse_s1[1,1] * mu[1,1] + 2 * inverse_s2[1,1] * mu[2,1]
    y1 <- 2 * inverse_s2[1,2] * mu[2,1] + 2 * inverse_s2[2,2] * mu[2,2] - 2 * inverse_s1[1,2] * mu[1,1] - 2 * inverse_s1[2,2] * mu[1,2]
    
    c <- -inverse_s2[1,1] * mu[2,1]^2 - 2 * inverse_s2[1,2] * mu[2,1] * mu[2,2] - inverse_s2[2,2] * mu[2,2]^2 + 
      inverse_s1[1,1] * mu[1,1]^2 + 2 * inverse_s1[1,2] * mu[1,1] * mu[1,2] + inverse_s1[2,2] * mu[1,2]^2 + 
      log(det(sigma1)) - log(det(sigma2)) - Py[1]/Py[2]
    
    func <- function(x, y) {
      x^2*x2 + y^2*y2 + x*y*xy + x*x1 + y*y1 + c
    }
    
    return(func)
  }
  
  getLyambda <- function(l1, l2, P1, P2) {
    log((l1*P1)/(l2*P2))
  }
  
  draw_plot <- function(xl, mu, sigma, Py, l1, l2) {
    x <- seq(min(mu[,1]) - 5, max(mu[,1]) + 5, length.out = 100)
    y <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    
    func <- discriminant(mu, sigma, Py)
    z <- outer(x, y, func)
    
    lyambda <- getLyambda(l1, l2, Py[1], Py[2])
    
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 3, levels = lyambda, col = "black", drawlabels = F, add = T)
  }
  
  len <- len1 + len2
  
  classes <- unique(xl[,ncol(xl)])
  
  draw_plot(xl, mu, sigma, Py, l1, l2)
}

```

Визуализация работы алгоритма при помощи <a href="https://kompot-vjacovich.shinyapps.io/Plug-in/">shiny</a>

![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/Plug-in/Classification_Plug-in.png)

<a href="#content">Вернуться к содержанию</a>

### <a name="LDF"></a>Линейный дискриминант Фишера
<b>ЛДФ</b> основан на <b>подстановочном</b> алгоритме с предположением, что ковариационные матрицы классов равны. Отсюда следует, что разделяющая поверхность вырождается в <b>прямую</b>. Это условие в plug-in не выполнялось, так как разделяющая поверхность все равно была квадратичной (хоть и приближенной к прямой). 

Так как матрицы равны, можем оценить их все вместе:

![](https://camo.githubusercontent.com/4a72020f09915379b2f8f30765a758252ba3af76/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436861742537422535435369676d6125374425323025334425323025354366726163253742312537442537426c2532302d2532302537435925374325374425323025354363646f7425323025354373756d5f25374269253344312537442535452537426c253744253238785f692532302d2532302535436861742537422535436d752537445f795f69253239253238785f692532302d2532302535436861742537422535436d752537445f795f6925323925354554)

Разделяющая поверхность между двумя классами *s* и *t* задаётся следующим образом:

Реализация на R
```LDF <- function(xl, len1, len2, Py, mu1, mu2, sigma, l1, l2, map) {
  Nu <- function(ksi, mu, sigma) (1/(sigma*sqrt(2*pi)))*exp(-(ksi-mu)^2 / (2*sigma^2))
  
  classification <- function(x, classes, mu, sigma, Py, lyambda) {
    classSum <- rep(0, length(classes))
    names(classSum) <- classes
    sigma[1,2] <- sigma[2,1] <- 1
    newSigma <- rbind(c(sigma[1,1], sigma[1,2]), c(sigma[1,1], sigma[1,2]))
    
    for (i in 1:length(classes)) {
      tmpSum <- 0
      
      for (j in 1:length(x)) {
        tmP <- Nu(x[j], mu[i,j], newSigma[i,j])
        
        tmpSum <- tmpSum + log(tmP)
      }
      classSum[i] <- tmpSum + log(lyambda[i]*Py[i])
    }
    
    return(names(which.max(classSum)))
  }
  
  classify_all <- function(classes, mu, sigma, Py, lyambda) {
    classifiedObj <- c()
    
    for(i in seq(min(mu[,1]) - 5, max(mu[,1] + 5), 0.1)) {
      for(j in seq(min(mu[,2]) - 5, max(mu[,2] + 5), 0.1)) {
        classifiedObj <- rbind(classifiedObj, c(i, j, classification(c(i, j), classes, mu, sigma, Py, lyambda)))
      }
    }
    return(classifiedObj)
  }
  
  discriminant <- function(mu1, mu2, sigma, Py) {
    inverse <- solve(sigma)
    
    b <- inverse %*% t(mu1 - mu2)
    
    x1 <- b[1, 1]
    y1 <- b[2, 1] 
    m <- (mu1 + mu2)
    const <- c(m %*% b) / 2
    
    func <- function(x, y) {
      x*x1 + y*y1 - const
    }
    
    return(func)
  }
  
  getLyambda <- function(l1, l2, P1, P2) {
    log((l1*P1)/(l2*P2))
  }
  
  draw_plot <- function(xl, mu1, mu2, sigma, Py, l1, l2) {
    mu <- rbind(mu1, mu2)
    x <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    y <- seq(min(mu[, 2]) - 5, max(mu[, 2]) + 5, length.out = 100)
    
    func <- discriminant(mu1, mu2, sigma, Py)
    z <- outer(x, y, func)
    
    lyambda <- getLyambda(l1, l2, Py[1], Py[2])
    
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 2.5, type="l", levels = lyambda, col = "black", drawlabels = F, add = T)
  }
  
  draw_map <- function(xl, classifiedObj) {
    n <- ncol(xl)
    colors <- c("first"="red", "second"="green3")
    plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], 
         main = "Карта классификации нормального распределения", asp = 1)
    points(classifiedObj[, 1:(n-1)], pch = 21, col = colors[classifiedObj[, n]])
  }
  
  len <- len1 + len2
  
  if(!map) {
    draw_plot(xl, mu1, mu2, sigma, Py, l1, l2)
  }
  else {
    classes <- unique(xl[,ncol(xl)])
    lyambda <- c(l1, l2)
    mu <- rbind(mu1, mu2)
    classified <- classify_all(classes, mu, sigma, Py, lyambda)
    draw_map(xl, classified)
  }
  
}
```
Вероятность ошибки <b>Линейного дискриминанта Фишера</b> выражается через расстояние Махаланобиса между классами, в случае, когда классов два:

![](https://latex.codecogs.com/gif.latex?R%28a%29%20%3D%20%5CPhi%28-%5Cfrac%7B1%7D%7B2%7D%7C%7C%5Cmu_1%20-%20%5Cmu_2%7C%7C_%7B%5CSigma_1%7D%29)

где ![](https://latex.codecogs.com/gif.latex?%5CPhi%28x%29%20%3D%20%5Cnu%28x%3B0%2C1%29) - нормальное гауссовское распределение.

```R
getRisk <- function(mu1, mu2, sigma) {
  mah <- (mu1 - mu2) %*% solve(sigma) %*% t(mu1 - mu2)
  
  mah <- mah * -0.5
  res <- gausian(mah, 0, 1)
}

gausian <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}
```

Визуализация работы алгоритма при помощи <a href="https://kompot-vjacovich.shinyapps.io/LDFisher/">shiny</a>

![](https://github.com/kompot-Vjacovich/LABS/blob/master/Bayes/LDF/Classification_LDF.png)

<a href="#content">Вернуться к содержанию</a>

## <a name="Linear"></a>Линейные алгоритмы классификации
Пусть ![](https://latex.codecogs.com/gif.latex?X%20%3D%20%5Cmathbb%7BR%7D%5E%7Bn%7D%2C%20Y%20%3D%20%5Cleft%20%5C%7B%20-1%3B%20&plus;1%20%5Cright%20%5C%7D). **Линейным** называется следующий алгоритм классификации:

![](https://latex.codecogs.com/gif.latex?a%28x%2C%20w%29%20%3D%20sign%20%3Cw%2C%20x%3E) ,
где ![](https://latex.codecogs.com/gif.latex?w%20%5Cepsilon%20%5Cmathbb%7BR%7D%5En) -- вектор параметров.

Гиперплоскость, разделяющая классы в пространстве ![](https://latex.codecogs.com/gif.latex?%5Cmathbb%7BR%7D%5En), задаётся уравнением **<w, x> = 0**. Если вектор *x* находится по одну сторону гиперплоскости с её направляющим вектором *w*, то объект *x* относится к классу +1, иначе -- к классу -1.

Для того, чтобы подобрать оптимальный вектор параметров *w*, минимизирующий эмпирический риск ![](https://latex.codecogs.com/gif.latex?Q%28w%2C%20X%5El%29%20%3D%20%5Csum_%7Bi%20%3D%201%7D%5E%7Bl%7DL%28%3Cw%2C%20x_i%3Ey_i%29) ,

применяется **метод стохастического градиента**. В этом методе сначала выбирается начальное приближение для *w* (инициализируется небольшими случайными значениями *w = (-1/2n, 1/2n)*, где *n* -- число признаков *x*), затем запускается итерационный процесс, на каждом шаге которого вектор *w* сдвигается в направлении, противоположном направлению вектора градиента ![](https://latex.codecogs.com/gif.latex?Q%27%28w%2C%20X%5El%29).

Веса *w* изменяются следующим образом:

![](https://latex.codecogs.com/gif.latex?w%20%3D%20w%20-%20%5Ceta%20Q%27%28w%29), где ![](https://latex.codecogs.com/gif.latex?%5Ceta%20>%200) -- *темп обучения*.

Функционал аппроксимированного эмпирического риска *Q* оценивается следующим образом:

![](https://latex.codecogs.com/gif.latex?Q%20%3D%20%281%20-%20%5Clambda%29Q%20&plus;%20%5Clambda*%5Cvarepsilon_i), где ![](https://latex.codecogs.com/gif.latex?%5Cvarepsilon_i%20%3D%20L%28<w%2C%20x_i>y_i%29) -- ошибка алгоритма на случайном объекте ![](https://latex.codecogs.com/gif.latex?x_l) из *Xl*, ![](https://latex.codecogs.com/gif.latex?%5Clambda) -- параметр сглаживания (обычно полагается *1/l*)

Сдвигаемся до тех пор, пока вектор *w* не перестанет изменяться и/или функционал *Q* не стабилизируется. Важно заметить, что градиент вычисляется не на всех объектах обучающей выборки, а на случайном объекте (поэтому *"стохастический"*). При использовании метода стохастического градиента нужно нормализовать данные:

![](https://latex.codecogs.com/gif.latex?x%5Ej%20%3D%20%5Cfrac%7Bx%5Ej%20-%20x_%7Bmean%7D%5Ej%7D%7Bx%5Ej_%7Bsd%7D%7D)

В зависимости от функции потерь *L* в функционале эмпирического риска *Q* существуют разнообразные **линейные алгоритмы классификации**.

Для минимизации
![](http://latex.codecogs.com/svg.latex?Q%28w%29)
применяется __метод градиентного спуска__.

В начале выбирается некоторое _начальное приближение вектора весов_ _w_.
Не существует единого способа инициализации весов. Хорошей практикой считается
инициализировать веса случайными малыми значениями:
![](http://latex.codecogs.com/svg.latex?w_j%3A%3D%5Ctext%7Brandom%7D%28-%5Cfrac%7B1%7D%7B2n%7D%2C&plus;%5Cfrac%7B1%7D%7B2n%7D%29)
, где _n_ – количество признаков _x_.

Далее высчитывается _текущая оценка функционала_
![](http://latex.codecogs.com/svg.latex?Q%3A%3D%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29)

Затем запускается итерационный процесс, на каждом шаге которого вектор _w_
изменяется в сторону наиболее быстрого убывания _Q_. Это направление противоположно
вектору градиента
![](http://latex.codecogs.com/svg.latex?Q%27%28w%29). Соответственно веса меняются по
правилу:

![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%20Q%27%28w%29)

или

![](http://latex.codecogs.com/svg.latex?w%3A%3Dw-%5Ceta%5Csum_%7Bi%3D1%7D%5E%7B%5Cell%7D%5Cmathcal%7BL%7D%27%28%5Clangle%20w%2Cx_i%20%5Crangle%20y_i%29x_iy_i),

где
![](http://latex.codecogs.com/svg.latex?%5Ceta%3E0)
– __темп обучения__. Чтобы не проскочить локальный минимум темп обучания принято
полагать небольшим. Однако, при слишком маленьком его значении алгоритм будет
медленно сходится. Для лучшего результата мы будем постепенно уменьшенать его по ходу
итераций.

<b>Адаптивный линейный элемент(ADALINE)</b> в качестве функции потерь использует квадратичную функцию:

![](https://camo.githubusercontent.com/8972321c0046dede7d7689f0f75d795c710e90ea/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436d61746863616c2537424c2537442532384d2532392533442532384d2d31253239253545322533442532382535436c616e676c6525323077253243785f6925323025354372616e676c65253230795f692d3125323925354532)

<b>Функция обновления весов:</b>

![](https://camo.githubusercontent.com/aefe5920605d5b7b56b9ffe17f12f8816a79daae/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f77253344772d2535436574612532382535436c616e676c6525323077253243785f6925323025354372616e676c652d795f69253239785f69)

<b>Персептрон Розенблата</b> в качестве функции потерь использует квадратичную функцию:

![](https://camo.githubusercontent.com/97f2b6593c8f819f61676cb91aefc2a65784a8d4/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436d61746863616c2537424c2537442533442532382d4d2532395f2b2533442535436d61782532382d4d25324330253239)

<b>Функция обновления весов вычисляется по правило Хебба:</b>

![](https://camo.githubusercontent.com/90d33699a1b3302dc1879c2c7eb823ff940b63f0/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535437465787425374269662532302537442535436c616e676c6525323077253243785f6925323025354372616e676c65253230795f6925334330253230253543746578742537422532307468656e25323025374425323077253341253344772b253543657461253230785f69795f69)

<b>Логистическая регрессия</b> в качестве функции потерь использует квадратичную функцию:

![](https://camo.githubusercontent.com/ee365cf43e497d5a900ef9c367bb83d742bdaecc/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535436d61746863616c2537424c2537442532384d2532392532302533442532302535436c6f675f32253238312532302b253230652535452537422d4d253744253239)

<b>Функция обновления весов:</b>

![](https://camo.githubusercontent.com/c1094cd0e0fefcaa0a2608da4bee189773cd1201/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f77253230253341253344253230772b253543657461253230795f69785f692535437369676d612532382d2535436c616e676c6525323077253243785f6925323025354372616e676c65253230795f69253239)

где ![](https://camo.githubusercontent.com/a169c0ba965ef8fe5740bce9f2cd9d3ce47a5f38/687474703a2f2f6c617465782e636f6465636f67732e636f6d2f7376672e6c617465783f2535437369676d612532387a2532392533442535436672616325374231253744253742312b652535452537422d7a253744253744)

Реализация на R
```
normalize <- function(xl) {
  for (i in 1:(ncol(xl)-1)) xl[,i] <- (xl[,i] - mean(xl[,i])) / sd(xl[,i])
  return (xl)
}

margin <- function(w, object, class) w %*% as.matrix(object) * class

adaLoss <- function(m) (1-m)^2
adaW <- function(w, object, class, eta) w - c(eta) * (w %*% object - class) %*% object

persLoss <- function(m) max(-m,0)
persW <- function(w, object, class, eta) w + c(eta) * object * class

logLoss <- function(m) log2(1 + exp(-m))
logW <- function(w, object, class, eta) w + c(eta) * object * class * sigmaZ(c(w %*% object) * class)

sigmaZ <- function(z) 1 / (1 + exp(-z))

gradient <- function(xl, eta, lambda, rule, loss_function, max_cnt, eps_q) {
  l <- nrow(xl)
  n <- ncol(xl)
  w <- matrix(c(runif(n-1, -1/(2*(n-1)), 1/(2*(n-1)))), 1, 3)
  objects <- xl[,-n]
  classes <- xl[, n]
  q <- sum(sapply(1:l, function(i) loss_function(margin(w, objects[i,], classes[i])))) 
  q_full <- matrix(q, 1, 1)
  
  cnt <- 0
  while (T) {
    cnt <- cnt + 1
    margins <- sapply(1:l, function(i) margin(w[cnt,], objects[i,], classes[i]))
    errors <- which(margins < 0)
    
    if (length(errors) > 0) rand <- sample(errors, 1)
    else rand <- sample(1:l, 1)
    
    eps <- loss_function(margin(w[cnt,], objects[rand,], classes[rand]))
    
    eta <- 1 / (objects[rand,] %*% objects[rand,])^2

    w <- rbind(w, rule(w[cnt,], objects[rand,], classes[rand], eta))
    
    q_prev <- q
    q <- (1 - lambda) * q + lambda * eps
    q_full <- rbind(q_full, q)
    
    if (abs(q_prev - q) / max(q_prev, q) <= eps_q) break
    else if (cnt == max_cnt) break
  }
  w <- cbind(w, q_full)
  return (w)
}
```

Визуализация работы линейных алгоритмов при помощи <a href="https://kompot-vjacovich.shinyapps.io/Linear/">shiny</a>

![](https://github.com/kompot-Vjacovich/LABS/blob/master/Linear/Classification_linear.png)

<a href="#content">Вернуться к содержанию</a>
