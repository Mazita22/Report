# Report
 Analise de dados e isights 

---
title: "Relatório BI"
date: "`r Sys.Date()`"
author: Atanasio Mazita
output:
  rmdformats::robobook:
    code_folding: show
    toc: 3
    thumbnails: false
    lightbox: true
    gallery: false
    cache: false
    highlight: kate
    fig_width: 10
    fig_height: 6
---

```{r config, include=FALSE}
# bibliotecas --- modificar sempre que acrescentar uma biblioteca
bibliotecas <- c("knitr", "rmdformats", "lubridate", "ggplot2","dplyr", "DT", "gt",
                 "flextable", "plotly", "gganimate")
for (bibl in bibliotecas) {
        library(bibl, character.only = TRUE)
}
rm(bibl, bibliotecas)

# knit: opções globais --- não modificar!
options(max.print="80")
opts_chunk$set(echo=FALSE,
	           cache=FALSE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=TRUE)
opts_knit$set(width=80)
```

## Autor

-   
-   Autor
    +  Atanásio Mazita
    

## Introdução

A empresa comercializa equipamentos de ciclismo, bicicletas, acessórios e vestuário adequados à prática do desporto. <br>
O relatório incide sobre o período que vai de 01-07-2016 a 31-07-2019.<br>
O portefólio da empresa é constituído 397 produtos, divididos em 4 categorias e 37 subcategorias. <br>
As vendas repartem-se por 10 territórios. <br>
<br>

De seguida apresenta-se uma breve descrição dos dados demográficos dos clientes, e dados para os diferentes departamentos da empresa.



```{r data_loading, include=FALSE}
data_files <- c("customers.Rda",
                "calendar.Rda",
                "productCategory.Rda",
                "products.Rda", "productSubCategory.Rda", 
                "territory.Rda", 
                "factSales.Rda", "facts_dims.Rda"
                )
for (dfiles in data_files) {
    load(dfiles)
}
rm(dfiles,data_files)
```

<br>




## Departamento Marketing

Abaixo estão representados vários gráficos que permitem ao Departamento de Marketing tomar decisões informadas e estratégicas.

### Perfil dos clientes

-   **Estado Civil**

No pie chart abaixo, podemos perceber que cada secção do gráfico representa a percentagem da população que corresponde a uma categoria específica de estado civil. Neste caso específico podemos distinguir entre Casados e Solteiros.
<br>
<br>

```{r freq_table_marital_status, echo=FALSE}

marital_status_count <- table(facts_dims$MaritalStatus)

percentages <- round(prop.table(marital_status_count) * 100, 1)

colors <- c("lightgreen", "darkgreen")
labels <- c("Solteiros", "Casados")

pie <- ggplot(data.frame(marital_status_count), aes(x = "", y = Freq, fill = factor(marital_status_count))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    labs(title = "Distribuicao entre Solteiros e Casados",
         fill = "Estado Civil") +
    scale_fill_manual(values = colors, labels = labels) +  
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")) +  
    geom_text(aes(label = paste(percentages, "%", sep = "")),
              position = position_stack(vjust = 0.5))

pie

```



- O gráfico seguinte está subdivido em 6 gráficos - um por cada país para o qual a empresa vende - e demonstra a quantidade de filhos por cliente.

```{r, echo=FALSE}
ggplot(facts_dims, aes(x = NumberChildrenAtHome, fill = Country)) +
    geom_bar() +
    labs(x = "Numero de Filhos", y = "Quantidade") +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~Country)
    
```

<br>

-   **Profissão do cliente**

No gráfico abaixo apresentado podemos visualizar a distribuição da quantidade de clientes em diferentes profissões.


```{r, echo = FALSE}

facts_dims$Occupation <- as.factor(facts_dims$Occupation)

ggplot(facts_dims, aes(x = Occupation)) +
    geom_bar(fill = "darkgreen", width = 0.4) +
    theme_classic() +
    labs(x = "Profissao", y = "Quantidade de clientes", title = "Profissao do Cliente") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

```

<br>

<br>

 - **Género por País**

É representado abaixo um gráfico de barras através do qual se pretende demonstrar a distribuição de género por país.

```{r, echo = FALSE}
ggplot(facts_dims, aes(x = Gender, fill = Gender)) +
    geom_bar(position = "dodge", width = 0.4) +
    
    facet_wrap(~Country) +
    labs(
        title = "Genero por Pais",
        x = "Pais",
        y = "Quantidade"
    ) +
    theme_classic()+
    theme(
        legend.position = "none",
        panel.spacing = unit(1, "lines"),
        strip.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        strip.background = element_rect(fill="white", colour="darkgreen", size=0.5)
    ) +
    scale_fill_manual(values = c("lightgreen", "darkgreen"))

```
<br>


-   **Numero de Vendas distribuidas por Profissão**
   
O gráfico mostra a distribuição percentual das vendas entre as diferentes profissões dos clientes da empresa.

```{r, echo=FALSE}

sales_by_CK <- customers[, c("Occupation", "CustomerKey")] %>%
    group_by(Occupation) %>%
    summarise(total_sales = n(), .groups = "drop")

donut_chart <- ggplot(sales_by_CK, aes(x = "", y = total_sales, fill = Occupation)) +
    geom_bar(width = 0.5, stat = "identity") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "right") +
    labs(title = "Vendas por Profissao") +
    scale_fill_manual(values = c("darkgreen", "lightgreen", "green", "grey", "darkgrey")) +
    geom_text(aes(label = paste0(round(total_sales/sum(total_sales) * 100), "%")), 
              position = position_stack(vjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(axis.text = element_blank(), 
          axis.title = element_blank())

donut_chart

```

   

<br> <br>

## Departamento de Vendas 

Nos gráficos que se seguem estão representados dados a ser partilhados com o departamento de vendas, com vista a um aumento de desempenho por parte da equipa..


-   **Vendas por Trimestre**

No gráfico abaixo estão representadas as vendas por trimestre, divididas pelos 4 anos que o relatório contempla. 

```{r, echo=FALSE}

ggplot(facts_dims, aes(x = CalendarQuarter)) +
    geom_bar(width = 0.5, fill = "darkgreen") + 
    facet_wrap(~ CalendarYear) +
    labs(title = "Vendas Trimestrais", x = "Trimestre", y = "Vendas Totais") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title.y = element_text(size = 12),  
          axis.title.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"))  
```


-  **Categorias mais vendidas por país**

No seguinte gráfico estão representadas as categorias de produto mais vendidas por país.
  
```{r, echo=FALSE}
ggplot(facts_dims, aes(x = Category, fill = Country)) + 
    geom_bar(position = "dodge") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x="Categoria", y="Quantidade Vendida", fill="Pais")

```


-  **Frequencia de produtos**


  
```{r, echo=FALSE}
product_counts <- table(products$ProductName)

top_products_df <- data.frame(ProductName = names(product_counts)[1:4],
                              Frequency = as.numeric(product_counts[1:4]))

flextable(top_products_df)



```



## Departamento Financeiro

As informações que se seguem têm como objetivo providenciar informação para o departamento financeiro.

-   **Vendas ao longo do tempo**

O gráfico que se segue demonstra a tendência das vendas ao longo dos anos.

```{r, echo=FALSE}
sales_over_time <- aggregate(facts_dims$SalesAmount, by=list(OrderDate=facts_dims$OrderDate), FUN=sum)

ggplot(sales_over_time, aes(x = OrderDate, y = x)) +
    geom_line() +
    labs(x = " ", y = "Vendas", title = "Vendas ao Longo do Tempo") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

```

-  **Rentabilidade por Produto**

Podemos verificar na tabela abaixo os quatro produtos mais vendidos da empresa e respetivas quantidades vendidas. A tabela deverá ser relacionada com o gráfico que lhe segue.
  

```{r, echo=FALSE}
top_customers <- facts_dims[, c("ProductName", "ProductKey", "SalesAmount")] %>%
    group_by(ProductName) %>%
    summarise(total_sales = sum(SalesAmount), .groups = "drop") %>%
    top_n(4, total_sales) %>%
    arrange(desc(total_sales))

flextable(top_customers)
```


 
```{r, echo=FALSE}

products_counts <- table(products$ProductKey)

sorted_counts <- sort(product_counts, decreasing = TRUE)

top_product_counts <- sorted_counts[1:4]

top_products_df <- data.frame(ProductName = names(top_product_counts),
                              Frequency = as.numeric(top_product_counts))

cores_personalizadas <- c("darkgreen", "lightgreen", "green", "grey")  

total <- sum(top_products_df$Frequency)
percentagens <- round((top_products_df$Frequency / total) * 100, 1)

top_products_df$Percentagem <- percentagens

ggplot(top_products_df, aes(x = 1, y = Frequency, fill = ProductName)) +
    geom_bar(stat = "identity", width = 0.5) +  
    geom_text(aes(label = paste0(Percentagem, "%")), position = position_stack(vjust = 0.5)) +  
    coord_polar(theta = "y") +
    xlim(c(0.5, 1.5)) +
    scale_fill_manual(values = cores_personalizadas) +
    theme_void() +
    theme(legend.position = "right") +
    labs(title = "Top 4 Produtos Mais Frequentes")


```



-   **Conclusões**

blá blá

# Apêndice

## 1. Dicionário do *data mart*

### dimensão `calendar`

+   **ID** : chave primária (*numeric*)\
+   **Date** : data (dd/mm/yyyy) da transacção (*date*)
+   **DayNumberOfWeek** : dia da semana (_num_)
+   **DayName** : nome do dia da semana (_character_)
+   **MonthName** : nome do mês (inglês) (_chr_)
+   **MonthNumberofYear** : numero do mês (_num_)
+   **DayNumberOfYear** : numero do dia do ano (_num_)
+   **WeekNumberOfYear** : numero da semana do ano (_num_)
+   **CalendarQuarter** : trimestre do ano (_num_)
+   **CalendarYear** : ano (_num_)
+   **DayNumberOfMonth** : dia do mês (_num_)


### dimensão `customers`

+   **CustomerKey** : chave primária número de cliente (_num_)
+   **GeographyKey** : código do país (_num_)
+   **Name** : nome do cliente (_character_)
+   **BirthDate** : data de nascimento (_num_)
+   **MaritalStatus** : estado civil (_character_)
+   **Gender** : género (_character_)
+   **YearlyIncome** : remuneração anual (_num_)
+   **NumberChildernAtHome** : número de filhos (_num_)
+   **Occupation** : profissão (_character_)
+   **HouseOwnerFlag** : proprietério de imóvel? (_num_)
+   **NumberCarsOwned** : número de carros (_num_)
+   **AddressLine** : morada (_character_)
+   **Phone** : número de telefone (_num_)
+   **DateFirstPurchase** : data da primeira compra (_date_)


### dimensão `factSales`
 
