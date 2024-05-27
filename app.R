#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Edité este comentario


library(shiny)
library(shinythemes) 
library(car) 
library(ggplot2) 


    
    
    # Define UI for application that draws a histogram
    ui <- fluidPage(theme = shinytheme("slate"),
        
        # Application title
        titlePanel("Normalidad"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                actionButton(inputId = "generar",
                             label =  "Generar",
                             class = "btn-success"),
                
                
                sliderInput(inputId = "replicas",
                            label = "Cantidad de replicas por tratamiento: ",
                            min = 6,
                            max = 45,
                            value = 6,
                            step = 3),

                sliderInput(inputId = "media",
                            label = "Media 1",
                            min = 0,
                            max = 1,
                            value = 0.50),
                
                sliderInput(inputId = "media1",
                            label = "Media 2",
                            min = 0,
                            max = 1,
                            value = 0.50),
                
                sliderInput(inputId = "media2",
                            label = "Media 3",
                            min = 0,
                            max = 1,
                            value = 0.50),
             
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                tabsetPanel(
                    tabPanel("QQPlot",plotOutput("distPlot")),
                    tabPanel("Intervalos de confianza",
                             plotOutput("grafico2")),
                    tabPanel("Potencia",tableOutput("potencia"))
                )
            )
        )
    )
    
    # Define server logic required to draw a histogram
    server <- function(input, output) {
        
        load("marco3a.Rdata")
       
        #En esta parte se generan los datos globales
        grafico1 <- eventReactive(input$generar, { #empieza parte de los graficos
            
            mu <- c(input$media,input$media1,input$media2)
            mu1 <- rep(mu,each=input$replicas)
            k <- length(mu)
            x <- factor(rep(1:3,each=input$replicas)) #generamos el factor
            n <- k*input$replicas

            #generacion de datos
            dat.normal <- rnorm(n = n,mean = mu1,sd = sqrt(0.2))
            dat.pois <- rpois(n=n,lambda = mu1)
            dat.nbinom <- rnbinom(n = n,size = 25,mu = mu1)

            dat.reales1 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
            dat.reales2 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
            dat.reales3 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)

            transformar1 <- function(m1){
                 x1 <- dat.reales1 - mean(dat.reales1)
                 x1 <- x1/sd(dat.reales1)
                 x1 <- x1 * sqrt(0.2)
                 x1 <- x1 + m1
                 x1
             }

             transformar2 <- function(m2){
                 x2 <- dat.reales2 - mean(dat.reales2)
                 x2 <- x2/sd(dat.reales2)
                 x2 <- x2 * sqrt(0.2)
                 x2 <- x2 + m2
                 x2
             }

             transformar3 <- function(m3){
                 x3 <- dat.reales3 - mean(dat.reales3)
                 x3 <- x3/sd(dat.reales3)
                 x3 <- x3 * sqrt(0.2)
                 x3 <- x3 + m3
                 x3
             }

            x1 <- transformar1(input$media)
            x2 <- transformar2(input$media1)
            x3 <- transformar3(input$media2)

            dat.reales <- cbind(c(x1,x2,x3))

            R <- sqrt(12*0.20)/2
            y1 = runif(n = input$replicas, min = input$media-R,max = input$media+R)
            y2 = runif(n = input$replicas, min = input$media1-R,max = input$media1+R)
            y3 = runif(n = input$replicas, min = input$media2-R,max = input$media2+R)

            dat.unif <- cbind(c(y1,y2,y3))

            #generacion de graficos
            par(mfrow=c(2,3),
                cex.lab=1.5, 
                cex.axis=1.5,
                cex.main=1.5, 
                cex.sub=1.5)
            qqPlot(lm(dat.normal~x)$residuals, 
                   ylab = "Cuantilos normales" ,
                   xlab = "Residuos del modelo" ,
                   main = "Modelo con distribucion normal")
            
            qqPlot(lm(dat.pois~x)$residuals,   
                   ylab = "Cuantilos normales" ,
                   xlab = "Residuos del modelo" ,
                   main = "Modelo con distribucion Poisson")
            
            qqPlot(lm(dat.nbinom~x)$residuals, 
                   ylab = "Cuantilos normales" ,
                   xlab = "Residuos del modelo" ,
                   main = "Modelo con distribucion Binomial Negativa")
            
            qqPlot(lm(dat.unif~x)$residuals,   
                   ylab = "Cuantilos normales" ,
                   xlab = "Residuos del modelo" ,
                   main = "Modelo con distribucion uniforme")
           
             qqPlot(lm(dat.reales~x)$residuals, 
                    ylab = "Cuantilos normales" ,
                    xlab = "Residuos del modelo" ,
                    main = "Modelo con distribucion del ingreso \n(datos reales)")
        })
        
        output$distPlot <- renderPlot(
            grafico1()
        ) #temina parte de graficos
        
        grafico_normal <- eventReactive(input$generar,{
            replicas <- input$replicas
            

# Normal ------------------------------------------------------------------
            mu <- c(input$media,input$media1,input$media2)
            k <- length(mu)
            n <- replicas*k
            
            mu1 <- rep(mu,each=replicas)
            
            x <- factor(rep(1:3,each=replicas)) #generamos el factor
            dat.normal <- rnorm(n,mu1,sqrt(0.2))
            
            mod.normal <- lm(dat.normal~x);anova(mod.normal)
            
            CMRes.normal <- anova(mod.normal)[2,3]
            glres <- anova(mod.normal)[2,1]
            
            (m <- tapply(dat.normal,x,mean))
            
            c1.c2 <- m[1]-m[2]
            c1.c3 <- m[1]-m[3]
            c2.c3 <- m[3]-m[2]
            
            d <- abs(c(c1.c2,c1.c3,c2.c3))
            gl1 <- length(d)
            ee <- sqrt(2*CMRes.normal/replicas)
            q <- d/ee
            
            t <- qt(1-0.05/(2*3),glres)
            
            (lim1 <- c(d[1]-t*ee,d[1]+t*ee))
            (lim2 <- c(d[2]-t*ee,d[2]+t*ee))
            (lim3 <- c(d[3]-t*ee,d[3]+t*ee))
   
            limites <- cbind(lim1,lim2,lim3)
            limites <- as.matrix(limites)

            limites <- t(limites)
            limites <- as.data.frame(limites)
            colnames(limites) <- c("min","max")
            limites$i = c("m12","m13","m23")
            limites$tipo = "Normal"

# Poisson -----------------------------------------------------------------

            mu <- c(input$media,input$media1,input$media2)
            k <- length(mu)
            replicas <- input$replicas
            n <- replicas*k
            mu1 <- rep(mu,each=replicas)
            x <- factor(rep(1:3,each=replicas))
            
            dat.pois <- rpois(n = n,lambda = mu1)
            mod.pois <- lm(dat.pois~x)
            CMRes.pois <- anova(mod.pois)[2,3]
            glres.pois <- anova(mod.pois)[2,1]
            mp <- tapply(dat.pois,x,mean)
            cp1.cp2 <- mp[1]-mp[2]
            cp1.cp3 <- mp[1]-mp[3]
            cp2.cp3 <- mp[3]-mp[2]
            
            dp <- abs(c(cp1.cp2,cp1.cp3,cp2.cp3))
            gl2 <- length(dp)
            eep <- sqrt(2*CMRes.pois/input$replicas)
            
            
            
            tp=qt(1-0.05/(2*3),glres.pois)
            (limp <- dp[1]-tp*eep)
            
            (lim1p <- c(dp[1]-tp*eep,dp[1]+tp*eep))
            (lim2p <- c(dp[2]-tp*eep,dp[2]+tp*eep))
            (lim3p <- c(dp[3]-tp*eep,dp[3]+tp*eep))
            
            limitesp <- cbind(lim1p,lim2p,lim3p)
            
            
            limitesp <- as.matrix(limitesp)
            
            
            limitesp <- t(limitesp)
            limitesp <- as.data.frame(limitesp)
            colnames(limitesp) <- c("min","max")
            limitesp$i = c("m12","m13","m23")
            limitesp$tipo = "Poisson"
            

# Binomial Negativa -------------------------------------------------------

            mu <- c(input$media,input$media1,input$media2)
            k <- length(mu)
            replicas <- input$replicas
            n <- replicas*k
            mu1 <- rep(mu,each=replicas)
            x <- factor(rep(1:3,each=replicas))
            
            dat.nb <- rnbinom(n = n,mu = mu1,size = 25)
            mod.nb <- lm(dat.nb~x)
            CMRes.nb <- anova(mod.nb)[2,3]
            glres.nb <- anova(mod.nb)[2,1]
            mnb <- tapply(dat.nb,x,mean)
            cnb1.cnb2 <- mnb[1]-mnb[2]
            cnb1.cnb3 <- mnb[1]-mnb[3]
            cnb2.cnb3 <- mnb[3]-mnb[2]
            
            dnb <- abs(c(cnb1.cnb2,cnb1.cnb3,cnb2.cnb3))
            gl3 <- length(dnb)
            eenb <- sqrt(2*CMRes.nb/input$replicas)
            qnb <- dnb/eenb
            
            tnb=qt(1-0.05/(2*3),glres.nb)
            (limp <- dnb[1]-tnb*eenb)
            
            (lim1nb <- c(dnb[1]-tnb*eenb,dnb[1]+tnb*eenb))
            (lim2nb <- c(dnb[2]-tnb*eenb,dnb[2]+tnb*eenb))
            (lim3nb <- c(dnb[3]-tnb*eenb,dnb[3]+tnb*eenb))
            
            limitesnb <- cbind(lim1nb,lim2nb,lim3nb)
            
            limitesnb <- as.matrix(limitesnb)
            
            limitesnb <- t(limitesnb)
            limitesnb <- as.data.frame(limitesnb)
            colnames(limitesnb) <- c("min","max")
            limitesnb$i = c("m12","m13","m23")
            limitesnb$tipo = "Binomial Negativa"

# Uniforme ----------------------------------------------------------------

            mu <- c(input$media,input$media1,input$media2)
            k <- length(mu)
            replicas <- input$replicas
            n <- replicas*k
            mu1 <- rep(mu,each=replicas)
            x <- factor(rep(1:3,each=replicas))
            
            R <- sqrt(12*0.20)/2
            y1 = runif(n = input$replicas, min = input$media-R,max = input$media+R)
            y2 = runif(n = input$replicas, min = input$media1-R,max = input$media1+R)
            y3 = runif(n = input$replicas, min = input$media2-R,max = input$media2+R)
            
            dat.u <- cbind(c(y1,y2,y3))
            
            mod.u <- lm(dat.u~x)
            CMRes.u <- anova(mod.u)[2,3]
            glres.u <- anova(mod.u)[2,1]
            m.u <- tapply(dat.u,x,mean)
            cu1.cu2 <- m.u[1]-m.u[2]
            cu1.cu3 <- m.u[1]-m.u[3]
            cu2.cu3 <- m.u[3]-m.u[2]
            
            du <- abs(c(cu1.cu2,cu1.cu3,cu2.cu3))
            gl4 <- length(du)
            eeu <- sqrt(2*CMRes.u/input$replicas)
            qu <- du/eeu
            
            
            tu=qt(1-0.05/(2*3),glres.u) 

            (lim1u <- c(du[1]-tu*eeu,du[1]+tu*eeu))
            (lim2u <- c(du[2]-tu*eeu,du[2]+tu*eeu))
            (lim3u <- c(du[3]-tu*eeu,du[3]+tu*eeu))
            
            limitesu <- cbind(lim1u,lim2u,lim3u)
            limitesu <- as.matrix(limitesu)
            limitesu <- t(limitesu)
            limitesu <- as.data.frame(limitesu)
            colnames(limitesu) <- c("min","max")
            limitesu$i = c("m12","m13","m23")
            limitesu$tipo = "Uniforme"

# Reales ------------------------------------------------------------------
            dat.reales1 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
            dat.reales2 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
            dat.reales3 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
            
            transformar1 <- function(m1){
                x1 <- dat.reales1 - mean(dat.reales1)
                x1 <- x1/sd(dat.reales1)
                x1 <- x1 * sqrt(0.2)
                x1 <- x1 + m1
                x1
            }
            
            transformar2 <- function(m2){
                x2 <- dat.reales2 - mean(dat.reales2)
                x2 <- x2/sd(dat.reales2)
                x2 <- x2 * sqrt(0.2)
                x2 <- x2 + m2
                x2
            }
            
            transformar3 <- function(m3){
                x3 <- dat.reales3 - mean(dat.reales3)
                x3 <- x3/sd(dat.reales3)
                x3 <- x3 * sqrt(0.2)
                x3 <- x3 + m3
                x3
            }
            
            x1 <- transformar1(input$media)
            x2 <- transformar2(input$media1)
            x3 <- transformar3(input$media2)
            
            dat.reales <- cbind(c(x1,x2,x3))
            
            mu <- c(input$media,input$media1,input$media2)
            k <- length(mu)
            replicas <- input$replicas
            n <- replicas*k
            mu1 <- rep(mu,each=replicas)
            x <- factor(rep(1:3,each=replicas))
            
            mod.real <- lm(dat.reales~x)
            CMRes.real <- anova(mod.real)[2,3]
            glres.real <- anova(mod.real)[2,1]
            m.real <- tapply(dat.reales,x,mean)
            c.real1.c.real2 <- m.real[1]-m.real[2]
            c.real1.c.real3 <- m.real[1]-m.real[3]
            c.real2.c.real3 <- m.real[3]-m.real[2]
            
            d.real <- abs(c(c.real1.c.real2,c.real1.c.real3,c.real2.c.real3))
            gl3 <- length(d.real)
            ee.real <- sqrt(2*CMRes.real/input$replicas)
            q.real <- d.real/ee.real
            
            t.real=qt(1-0.05/(2*3),glres.real)
            (limp <- d.real[1]-t.real*ee.real)
            
            (lim1.real <- c(d.real[1]-t.real*ee.real,d.real[1]+t.real*ee.real))
            (lim2.real <- c(d.real[2]-t.real*ee.real,d.real[2]+t.real*ee.real))
            (lim3.real <- c(d.real[3]-t.real*ee.real,d.real[3]+t.real*ee.real))
            
            limites.real <- cbind(lim1.real,lim2.real,lim3.real)
            
            limites.real <- as.matrix(limites.real)
            
            limites.real <- t(limites.real)
            limites.real <- as.data.frame(limites.real)
            colnames(limites.real) <- c("min","max")
            limites.real$i = c("m12","m13","m23")
            limites.real$tipo = "Datos Reales"
            
            linea1 <- c(input$media, input$media1)
            linea1 <- max(linea1) - min(linea1)
            
            linea2 <- c(input$media, input$media2)
            linea2 <- max(linea2) - min(linea2)
            
            linea3 <- c(input$media1, input$media2)
            linea3 <- max(linea3) - min(linea3)
    
            limites <- rbind(limites, limitesp, limitesnb, limitesu, limites.real)
            limites$media <- abs(c(input$media-input$media1,input$media - input$media2, input$media1 - input$media2))
            
            StatMeanLine <- ggproto("StatMeanLine", Stat,
                                    compute_group = function(data, scales) {
                                        transform(data, yintercept=mean(y))
                                    },
                                    required_aes = c("x", "y")
            )
            
            stat_mean_line <- function(mapping = NULL, data = NULL, geom = "hline",
                                       position = "identity", na.rm = FALSE, show.legend = NA, 
                                       inherit.aes = TRUE, ...) {
                layer(
                    stat = StatMeanLine, data = data, mapping = mapping, geom = geom, 
                    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                    params = list(na.rm = na.rm, ...)
                )
            }
            
            
            ggplot(data = limites,
                   mapping = aes(x = tipo, color = tipo)) +
                geom_point(aes(y = min), size = 2)+ 
                geom_point(aes(y = max), size = 2) +
                geom_segment(aes(y = min, yend = max,
                                 x = tipo, xend = tipo),
                             size = 1) +
                facet_wrap(~i, nrow = 1, strip.position="bottom") +
                
                stat_mean_line(aes(y = media), color = "red") +
                
                geom_hline(yintercept = 0, 
                           color = "tomato",
                           linetype="dashed") +
                labs(x = "Comparaciones de las medias",
                     y = "Intervalos de confianza",
                     title = "Intervarlos de confianza con diferentes distribuciones",
                     color = "Tipo") +
                
                theme_classic() +
                theme(plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank())
        })

        output$grafico2 <- renderPlot({
            grafico_normal()
            
        })

        #Potencias:----
        tabla_potencia <- eventReactive(input$generar,{
            
            tabla.potencias <- matrix(rep(NA,6),nrow = 1,ncol = 6)
            tabla.potencias[1] <- 0.05
            
            #Normal
            med.normal = function(r, mu, var.error) {
                k = length(mu)
                n = r * k
                y = rnorm(n, rep(mu,each=r), sqrt(var.error))
                x = factor(rep(1:k, each=r))
                mod.norm = aov(y ~ x)
                p.norm = anova(mod.norm)[1, 5]
                return(p.norm)
            }

            #Poisson
            med.pois = function (r, mu) {
                k = length(mu)
                n = r * k
                y = rpois(n, rep(mu,each=r))
                x = factor(rep(1:k, each=r))
                mod.pois = aov(y ~ x)
                p.pois = anova(mod.pois)[1, 5]
                return(p.pois)
            }

            #Binomial negativa
            med.nbinom = function (r, mu) {
                k = length(mu)
                n = r * k
                y = rnbinom(n = n, mu = rep(mu,each=r),size = 25)
                x = factor(rep(1:k, each=r))
                mod.nbinom = aov(y ~ x)
                p.nbinom = anova(mod.nbinom)[1, 5]
                return(p.nbinom)
            }
            
            #Uniforme
            med.unif = function (r,k) {
                n = r * k
                R <- sqrt(12*0.20)/2
                y1 = runif(n = r, min = input$media-R,max = input$media+R)
                y2 = runif(n = r, min = input$media1-R,max = input$media1+R)
                y3 = runif(n = r, min = input$media2-R,max = input$media2+R)
                (y <- cbind(c(y1,y2,y3)))

                x = factor(rep(1:k, each=r))
                mod.unif = aov(y ~ x)
                p.unif = anova(mod.unif)[1, 5]
                return(p.unif)
            }
            
            
# Datos reales ------------------------------------------------------------

            med.real = function (r,k) {
                dat.reales1 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
                dat.reales2 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
                dat.reales3 <- sample(x = marco3a$ingresoph,size = input$replicas,replace = FALSE)
                
                transformar1 <- function(m1){
                    x1 <- dat.reales1 - mean(dat.reales1)
                    x1 <- x1/sd(dat.reales1)
                    x1 <- x1 * sqrt(0.2)
                    x1 <- x1 + m1
                    x1
                }
                
                transformar2 <- function(m2){
                    x2 <- dat.reales2 - mean(dat.reales2)
                    x2 <- x2/sd(dat.reales2)
                    x2 <- x2 * sqrt(0.2)
                    x2 <- x2 + m2
                    x2
                }
                
                transformar3 <- function(m3){
                    x3 <- dat.reales3 - mean(dat.reales3)
                    x3 <- x3/sd(dat.reales3)
                    x3 <- x3 * sqrt(0.2)
                    x3 <- x3 + m3
                    x3
                }
                x1 <- transformar1(input$media)
                x2 <- transformar2(input$media1)
                x3 <- transformar3(input$media2)
                dat.reales <- cbind(c(x1,x2,x3))
                n = input$replicas * k 
                x = factor(rep(1:k, each=r))
                mod.real = aov(dat.reales ~ x)
                p.real = anova(mod.real)[1, 5]
                return(p.real)
            }
            
            mu1 <- c(input$media,input$media1,input$media2)
            
            prob.norm <- c()
            prob.pois <- c()
            prob.nbinom <- c()
            prob.unif <- c()
            prob.real <- c()
            
            
            
            for (i in 1:1000) {
                prob.norm[i] <- med.normal(r=input$replicas, mu = mu1,var.error = 0.20)
                prob.pois[i] <- med.pois(r=input$replicas, mu = mu1)
                prob.nbinom[i] <- med.nbinom(r=input$replicas,mu = mu1)
                prob.unif[i] <- med.unif(r=input$replicas,k=3)
                prob.real[i] <- med.real(r=input$replicas,k=3)
                
            }
            tabla.potencias[2] <- mean(na.omit(prob.norm)<0.05)
            tabla.potencias[3] <- mean(na.omit(prob.pois)<0.05)
            tabla.potencias[4] <- mean(na.omit(prob.nbinom)<0.05)
            tabla.potencias[5] <- mean(na.omit(prob.unif)<0.05)
            tabla.potencias[6] <- mean(na.omit(prob.real)<0.05)
            colnames(tabla.potencias) <- c("Significancia","Normal","Poisson","Binomial negativa","Uniforme","Reales")
            
            tabla.potencias
            

        })
        output$potencia <- renderTable({
            tabla_potencia()
        })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    

