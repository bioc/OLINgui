OLINgui <- function(){

##### DEFINITION OF WIDGETS
nenv <- new.env(hash=TRUE,parent=parent.frame())
gui <- tktoplevel()
tktitle(gui) <- "GUI for OLIN package"
#####
# DATA LOADING 
width1 <- 15
gui.11l <-   tklabel(gui,text="DATA LOADING",fg="red")
gui.12l <-   tklabel(gui,text="Marray object")
gui.12ba <-  tkbutton(gui,text="Browse objects",width=width1)
gui.12bb <-  tkbutton(gui,text="Browse files",width=width1)
gui.13l <-   tklabel(gui,text="XY list")
gui.13ba <-  tkbutton(gui,text="Browse objects",width=width1)
gui.13bb <-  tkbutton(gui,text="Browse files",width=width1)

maobjname <- tclVar(); maobjname <- "No object loaded";
gui.14ta <-  tktext(gui,width=width1+2,height=1);tkinsert(gui.14ta,"0.0",maobjname)
xyname <- tclVar(); xyname <- "No XY list loaded";
gui.14tb <-  tktext(gui,width=width1+2,height=1);tkinsert(gui.14tb,"0.0",xyname);
gui.15l  <-  tklabel(gui,text="________________________________________")

#########
# VISUALISATION
width.2 <- 10
gui.21l <- tklabel(gui,text="VISUALISATION",fg="red")
gui.22l <- tklabel(gui,text="Array index:")
gui.22e <- tkentry(gui,width=8)
visuindex <- tclVar(); tclvalue(visuindex) <- 1 
tkinsert(gui.22e,"end",tclvalue(visuindex))
gui.23ba <- tkbutton(gui,text="Fgbg.visu",width=width.2,bg="gray")
gui.23bb <- tkbutton(gui,text="MA plot",width=width.2,bg="gray")
gui.23bc <- tkbutton(gui,text="MXY plot",width=width.2,bg="gray")
gui.23bd <- tkbutton(gui,text="MXY2 plot",width=width.2,bg="gray")
gui.24l <- tklabel(gui,text="_______________________________________")

#########
# NORMALISATION 
gui.31l <- tklabel(gui,text="NORMALISATION",fg="red")
norm <- tclVar(); tclvalue(norm) <- "olin";
gui.32ra <- tkradiobutton(gui,text="OLIN",variable=norm,value="olin")
gui.32rb <- tkradiobutton(gui,text="OSLIN",variable=norm,value="oslin")
alphavar <- tclVar(); alphavar <- seq(0.1,1,0.1);
gui.33l <- tklabel(gui,text="Smoothing:")
gui.33e <- tkentry(gui)
tkinsert(gui.33e,"end",alphavar)
scalingvar <- tclVar(); scalingvar <- c(0.05, 0.1, 0.5, 1, 2, 10, 20);
gui.34l <- tklabel(gui,text="Scaling:")
gui.34e <- tkentry(gui)
tkinsert(gui.34e,"end",scalingvar)
gui.35l <- tklabel(gui,text="Iterations:")
iteravar <- tclVar(); iteravar <- 3;
gui.35e <- tkentry(gui,width=3)
tkinsert(gui.35e,"end",iteravar)
weightsvar <- tclVar(); tclvalue(weightsvar) <- "0";  basvar <- tclVar(); tclvalue(basvar) <- "0";  
gui.36ca <- tkcheckbutton(gui,text="Weights",variable=weightsvar)
gui.36cb <- tkcheckbutton(gui,text="BAS",variable=basvar)
gui.37b <- tkbutton(gui,text="Normalise",width=width.2,bg="gray")
gui.38b <- tkbutton(gui,text="Save object",width=width.2)
gui.38bb <- tkbutton(gui,text="Export object",width=width.2)
gui.39l <- tklabel(gui,text="_____________________________________")

#########
# STATISTICIS
width.4 <- 10
gui.41l <- tklabel(gui,text="STATISTICS",fg="red")
gui.42l <- tklabel(gui,text="Array index:")
gui.42e <- tkentry(gui,width=8)
statsindex <- tclVar(); tclvalue(statsindex) <- "1"
tkinsert(gui.42e,"end",tclvalue(statsindex))
 
gui.42ba <- tkbutton(gui,text="ANOVA Plate",width=width.4,bg="gray")
gui.42bb <- tkbutton(gui,text="ANOVA Pin",width=width.4,bg="gray")

gui.43ba <- tkbutton(gui,text="ANOVA int",width=width.4,bg="gray")
gui.43bb <- tkbutton(gui,text="FDR int",width=width.4,bg="gray")
gui.43bc <- tkbutton(gui,text="p int",width=width.4,bg="gray")
gui.44ba <- tkbutton(gui,text="ANOVA spa",width=width.4,bg="gray")
gui.44bb <- tkbutton(gui,text="FDR spa",width=width.4,bg="gray")
gui.44bc <- tkbutton(gui,text="p spa",width=width.4,bg="gray")
statsvisu <- tclVar(); statsvisu <- "1";
gui.45c  <- tkcheckbutton(gui,text="Visualisation of results",variable=statsvisu)
gui.46b <- tkbutton(gui,text="Save results")
gui.46bb <- tkbutton(gui,text="Export results")
gui.47l <- tklabel(gui,text="==========================")

gui.51b <- tkbutton(gui,text="Quit",width=width.2)
gui.52l <- tklabel(gui,text="")
#####


tkgrid(gui.11l,columnspan=4,column=0,sticky="w")
tkgrid(gui.12l,gui.13l)
tkgrid.configure(gui.13l,column=2)
tkgrid(gui.12ba,gui.13ba)
tkgrid.configure(gui.13ba,column=2)
tkgrid(gui.12bb,gui.13bb)
tkgrid.configure(gui.13bb,column=2)
tkgrid(gui.14ta,gui.14tb)
tkgrid.configure(gui.14tb,column=2,sticky="w")
tkgrid(gui.15l)
tkgrid.configure(gui.15l,columnspan=3)

tkgrid(gui.21l,columnspan=4,column=0,sticky="w")
#tkgrid(gui.22l,gui.22e)
#tkgrid.configure(gui.22l,sticky="e")
#tkgrid.configure(gui.22e,sticky="w")
tkgrid(gui.23ba,gui.22l,gui.23bc)
tkgrid.configure(gui.23bc,column=2)
tkgrid(gui.23bb,gui.22e,gui.23bd)
tkgrid.configure(gui.23bd,column=2)
tkgrid(gui.24l)
tkgrid.configure(gui.24l,columnspan=3)


tkgrid(gui.41l,columnspan=4,column=0,sticky="w")
#tkgrid(gui.42l,gui.42e)
#tkgrid.configure(gui.42l,sticky="e")
#tkgrid.configure(gui.42e,sticky="w")
tkgrid(gui.42ba,gui.42l,gui.42bb)
tkgrid.configure(gui.42bb,column=2)

tkgrid(gui.43ba,gui.42e,gui.44ba)
tkgrid.configure(gui.44ba,column=2)

tkgrid(gui.43bb,gui.44bb)
tkgrid.configure(gui.44bb,column=2)

tkgrid(gui.43bc,gui.44bc)
tkgrid.configure(gui.44bc,column=2)

tkgrid(gui.45c)
tkgrid.configure(gui.45c,columnspan=3)
tkgrid(gui.46b,gui.46bb)
tkgrid.configure(gui.46bb,column=2)
tkgrid(gui.39l)
tkgrid.configure(gui.39l,columnspan=3)

tkgrid(gui.31l,columnspan=4,column=0,sticky="w")
tkgrid(gui.32ra,gui.32rb)
tkgrid.configure(gui.32ra,column=1)
tkgrid.configure(gui.32rb,column=2)
tkgrid(gui.33l,gui.33e)
tkgrid.configure(gui.33e,column=1,columnspan=2,sticky="w")
tkgrid.configure(gui.33l,sticky="e")
tkgrid(gui.34l,gui.34e)
tkgrid.configure(gui.34e,column=1,columnspan=2,sticky="w")
tkgrid.configure(gui.34l,sticky="e")
tkgrid(gui.35l,gui.35e)
tkgrid.configure(gui.35l,sticky="e")
tkgrid.configure(gui.35e,column=1,columnspan=2,sticky="w")
tkgrid(gui.36ca,gui.36cb)
tkgrid.configure(gui.36ca,column=1)
tkgrid.configure(gui.36cb,column=2)
tkgrid(gui.37b)
tkgrid.configure(gui.37b,column=1)
tkgrid(gui.38b,gui.38bb)
tkgrid.configure(gui.38bb,column=2)


tkgrid(gui.47l)
tkgrid.configure(gui.47l,columnspan=3)

tkgrid(gui.51b)
tkgrid.configure(gui.51b,columnspan=3)
tkgrid(gui.52l)
#### CONFIGURATION

# MARRAY OBJECT LOADING
browseObject <- function(){
           tmp <- objectBrowser()
           if (!(is.null(tmp))){
              if ((class(tmp[[1]])=="marrayRaw")|(class(tmp[[1]])=="marrayNorm")){
              	assign("obj",tmp[[1]],nenv)
           	tkdelete(gui.14ta,"0.0","end")
           	tkinsert(gui.14ta,"0.0",attributes(tmp)$names[[1]])
           	rm(tmp) 
             } else {
               tkmessageBox(message="Object is not of marraryRaw or marrayNorm class",
                         title="Object is not loaded!")
              }
           }
}
tkconfigure(gui.12ba,command=browseObject)


browseFile <- function(){
            envtmp <- new.env(hash=TRUE,parent=parent.frame()) 
            file <- fileBrowser()[[1]]
            if (!(is.null(file))){
            tmp <- try(load(file,envtmp),TRUE)
            if (class(tmp)=="try-error"){  
               tkmessageBox(message=tmp[1])
               return()
            }
            if ((class(get(tmp,envtmp))=="marrayRaw")|(class(get(tmp,envtmp))=="marrayNorm")){
              tkdelete(gui.14ta,"0.0","end")
              tkinsert(gui.14ta,"0.0",tmp)
              assign("obj",get(tmp,envtmp),nenv)
             }else {
               tkmessageBox(message="Object is not of marraryRaw or marrayNorm class",
                         title="Object is not loaded!")
             }
            

            rm(list=ls(envtmp),envir=envtmp); rm(tmp);
         }  
}

tkconfigure(gui.12bb,command=browseFile)


### XY-LIST LOADING
browseObject2 <- function(){
           tmp <- objectBrowser()
           if (!is.list(tmp[[1]])|(length(tmp[[1]])!=2)|(!all(attributes(tmp[[1]])$names == c("X","Y")))){
                tkmessageBox(message="List is not of correct format")
                return()
            }
           if  (!(is.null(tmp))){
           assign("xy",tmp[[1]],nenv)
           tkdelete(gui.14tb,"0.0","end")
           tkinsert(gui.14tb,"0.0",attributes(tmp)$names[[1]])
           rm(tmp)
           }
}
tkconfigure(gui.13ba,command=browseObject2)

browseFile2 <- function(){
            envtmp <- new.env(hash=TRUE,parent=parent.frame()) 
            file <- fileBrowser()[[1]]
             if (!(is.null(file))){
             tmp <- try(load(file,envtmp),TRUE)
            if (class(tmp)=="try-error"){  
               tkmessageBox(message=tmp[1])
               return()
            }
            if (!all(attributes(get(tmp,envtmp))$names == c("X","Y"))){
                tkmessageBox(message="List is not of correct format")
                return()
            }
            tkdelete(gui.14tb,"0.0","end")
            tkinsert(gui.14tb,"0.0",tmp)
            assign("xy",get(tmp,envtmp),nenv)
            rm(list=ls(envtmp),envir=nenvtmp); rm(tmp);
            }
 }
tkconfigure(gui.13bb,command=browseFile2)


# VISUALISATION 

fgbgvisu <- function(){
             if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray object has been loaded.")
                  return()
             }
                         
             index <- as.integer(strsplit(tclvalue(tkget(gui.22e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.22e))," ")[[1]]))]
            
            if (length(index)==0){
               index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }

            if (class(get("obj",nenv))=="marrayRaw"){
            	for (i in index){
            	fgbg.visu(get("obj",nenv)[,i],label=paste("Array",i))
             		if (length(index)>1){
            		tkmessageBox(message="Plotting next array?",title="Next?"  )              
             		}
                }
            }else {
            	gui.fgbg <- tktoplevel()
            	tktitle(gui.fgbg) <- "WARNING"
            	gui.fgbg.l <- tklabel(gui.fgbg,text="Object is not of class marrayraw",fg="red")
            	gui.fgbg.b <- tkbutton(gui.fgbg,text="OK")
            	des.gui.fgbg <- function(){
                	tkdestroy(gui.fgbg)
            	}
            	tkconfigure(gui.fgbg.b,command=des.gui.fgbg)
            	#tkgrid(gui.fgbg.l1);
            	tkgrid(gui.fgbg.l); tkgrid(gui.fgbg.b);
            	#tkgrid(gui.fgbg.l1)            
            }
  
}
tkconfigure(gui.23ba,command=fgbgvisu)

mavisu <- function(){
            if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray object has been loaded.")
                  return()
             }
                
             index <- as.integer(strsplit(tclvalue(tkget(gui.22e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.22e))," ")[[1]]))]
            
            if (length(index)==0){
            index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }

            for (i in index){
            plot(maA(get("obj",nenv)[,i]),maM(get("obj",nenv)[,i]),xlab="A",
                 ylab="M",main=paste("Array",i))
            if (length(index)>1){
              tkmessageBox(message="Plotting next array?",title="Next?"  )      
            }
 }
}           

tkconfigure(gui.23bb,command=mavisu)


mxyplot <- function(){
             if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray object has been loaded.")
                  return()
             }
                
             index <- as.integer(strsplit(tclvalue(tkget(gui.22e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.22e))," ")[[1]]))]
            
            if (length(index)==0){
            index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }

            for (i in index){
            mxy.plot(V=maM(get("obj",nenv)[,i]), 
                    Ngc=maNgc(get("obj",nenv)[,i]), 
                    Ngr=maNgr(get("obj",nenv)[,i]),
                    Nsc=maNsc(get("obj",nenv)[,i]), 
                    Nsr=maNsr(get("obj",nenv)[,i]),
                    main=paste("Array",i))
            if (length(index)>1){
                   tkmessageBox(message="Plotting next array?",title="Next?"  )         
            }

        }
}           

tkconfigure(gui.23bc,command=mxyplot)

mxy2plot <- function(){
             if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray object has been loaded.")
                  return()
             }
              if (!exists("xy",nenv)){
                  tkmessageBox(message="No XY list has been loaded.")
                  return()
             }
             
                
             index <- as.integer(strsplit(tclvalue(tkget(gui.22e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.22e))," ")[[1]]))]
            
            if (length(index)==0){
            index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }

            for (i in index){
            mxy2.plot(V=maM(get("obj",nenv)[,i]), 
                    X=get("xy",nenv)$X[,i],
                    Y= get("xy",nenv)$Y[,i],
                    Ngc=maNgc(get("obj",nenv)[,i]), 
                    Ngr=maNgr(get("obj",nenv)[,i]),
                    Nsc=maNsc(get("obj",nenv)[,i]), 
                    Nsr=maNsr(get("obj",nenv)[,i]),
                    main=paste("Array",i))
            if (length(index)>1){
                    tkmessageBox(message="Plotting next array?",title="Next?"  )            
           }           
           }           
}
tkconfigure(gui.23bd,command=mxy2plot)


#################################
# NORMALISATION

normalise <- function(){
           if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray object has been loaded.")
                  return()
           }
             
          SCALING <- FALSE; X <- NA; Y <- NA;WEIGHTS <- NA;
          if  (tclvalue(norm)=="oslin"){ # OLIN OR OSLIN PERFORMED?
               SCALING <- TRUE
          }          
         
          if (exists("xy",envir=nenv)){
              X <- get("xy",nenv)$X
              Y <- get("xy",nenv)$X
          }
          
          ALPHA    <- as.real(strsplit(tclvalue(tkget(gui.33e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.33e))," ")[[1]]))]
          SCALE  <- as.real(strsplit(tclvalue(tkget(gui.34e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.34e))," ")[[1]]))]
          
       
         # gui.wait <- tktoplevel()
         # tktitle(gui.wait) <- "Wait"
         # gui.wait.l <- tklabel(gui.wait,text="Please wait..",fg="red")
         # tkgrid(gui.wait.l)
          if (tclvalue(weightsvar)=="1"){ 
 		obj.norm <- olin(object=get("obj",nenv),X=X,Y=Y,
                      alpha=ALPHA,scale= SCALE,iter=as.integer(tclvalue(tkget(gui.35e))),
                      scaling=SCALING,weights= maW(get("obj",nenv)))
                      } else {
               obj.norm <- olin(object=get("obj",nenv),X=X,Y=Y,
                      alpha=ALPHA,scale= SCALE,iter=as.integer(tclvalue(tkget(gui.35e))),
                      scaling=SCALING)
           }
          
                      
          if (tclvalue(basvar)=="1"){
            gui.bas <- tktoplevel()
            tktitle(gui.bas) <- "Options for BAS"        
            basopt <- tclVar(); tclvalue(basopt) <- "var";
            gui.bas.l <- tklabel(gui.bas,text="Methods for between-array scaling",fg="red")
            gui.bas.r1 <- tkradiobutton(gui.bas,text="Var",variable=basopt,value="var")
            gui.bas.r2 <- tkradiobutton(gui.bas,text="MAD",variable=basopt,value="mad")
            gui.bas.r3 <- tkradiobutton(gui.bas,text="QQ",variable=basopt,value="qq")
            gui.bas.b <- tkbutton(gui.bas,text="OK")
            
            tkgrid(gui.bas.l,columnspan=3,column=0,sticky="w")
            tkgrid(gui.bas.r1,gui.bas.r2,gui.bas.r3)
            tkgrid(gui.bas.b,column=1)
            des <- function(){
                   obj.norm <- bas(obj.norm,tclvalue(basopt))
                   assign("obj.norm",obj.norm,nenv)
                   tkdestroy(gui.bas)
            }
            tkconfigure(gui.bas.b,command=des)
            #tkwait.window(gui.bas)
            #obj.norm <- bas(obj.norm,tclvalue(basvar))
            
          }
         #  tkdestroy(gui.wait)
          #  assign("obj.norm",obj.norm,nenv)
         

}
tkconfigure(gui.37b,command=normalise)

exportnorm <- function(){
             if (!exists("obj.norm",nenv)){
                  tkmessageBox(message="OLIN has not been applied yet.")
                  return()
             }
             
            gui.export <-tktoplevel()
            tktitle(gui.export) <- "Choose"
            gui.export.l <- tklabel(gui.export,text="Choose name for normalised marrayNorm object:")
            tmp <- tclVar(); tclvalue(tmp) <- "obj.norm"; 
            gui.export.e <- tkentry(gui.export)
            tkinsert(gui.export.e,"end",tclvalue(tmp))
            gui.export.b <- tkbutton(gui.export,text="Export to global environment")
            tkgrid(gui.export.l);tkgrid(gui.export.e);tkgrid(gui.export.b)
            exportobj <- function(){ 
                      assign(as.character(tclvalue(tkget(gui.export.e))),get("obj.norm",nenv),envir=globalenv())
                      tkdestroy(gui.export)
             }
            tkconfigure(gui.export.b,command=exportobj)
 
}
tkconfigure(gui.38bb,command=exportnorm)

savenorm <- function(){ 
             if (!exists("obj.norm",nenv)){
                  tkmessageBox(message="OLIN has not been applied yet.")
                  return()
             }

             norm.olin <- get("obj.norm",nenv) 
             f <- tclvalue(tkgetSaveFile())
             if (f !=""){ 
             save(norm.olin,file=f)
             }
            }
tkconfigure(gui.38b,command=savenorm)

###############################################################
####### STATSISTICS
# ANOVA PIN

anovapi <- function(){
     if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }
    
      index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
      if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
          }
      obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
       runindex <- runindex + 1;
       obj.stats[[runindex]]  <- anovapin(get("obj",nenv),index[runindex]) 
                               
      

      if (tclvalue(statsvisu)=="1"){
          gui.anovapin.res <- tktoplevel()
          tktitle(gui.anovapin.res) <- paste("Results of ANOVA for array",index[[runindex]])
          gui.anovapin.res.l <- tklabel(gui.anovapin.res,text="Summary of ANOVA model:")
          gui.anovapin.res.t <- tktext(gui.anovapin.res)
          gui.anovapin.res.s <- tkscrollbar(gui.anovapin.res,command=function(...) tkyview(gui.anovapin.res.t,...))
          gui.anovapin.res.b <- tkbutton(gui.anovapin.res,text="Close")
          tkconfigure(gui.anovapin.res.t,yscrollcommand=function(...) tkset(gui.anovapin.res.s,...))
          tkpack(gui.anovapin.res.s,fill="y",side="right")
          tkpack(gui.anovapin.res.t,fill="both",expand=TRUE)
          tkpack(gui.anovapin.res.b) 
          
          tt <- textConnection("x","w")
          sink(tt)
          print(obj.stats[[runindex]])
          sink()
          tkinsert(gui.anovapin.res.t,"end",paste(x,"\n",collapse=""))
          tkconfigure(gui.anovapin.res.b,command=function(...) tkdestroy(gui.anovapin.res))
          tkwait.window(gui.anovapin.res)
          }
      }
     
     assign("obj.stats",obj.stats,nenv)
  }


tkconfigure(gui.42bb,command=anovapi)


# ANOVA PLATE

anovapl <- function(){
     if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }

  
      index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
      if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
          }
      obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
       runindex <- runindex + 1;
       obj.stats[[runindex]]  <- anovaplate(get("obj",nenv),index[runindex]) 
                               
      

      if (tclvalue(statsvisu)=="1"){
          gui.anovapl.res <- tktoplevel()
          tktitle(gui.anovapl.res) <- paste("Results of ANOVA for array",index[[runindex]])
          gui.anovapl.res.l <- tklabel(gui.anovapl.res,text="Summary of ANOVA model:")
          gui.anovapl.res.t <- tktext(gui.anovapl.res)
          gui.anovapl.res.s <- tkscrollbar(gui.anovapl.res,command=function(...) tkyview(gui.anovapl.res.t,...))
          gui.anovapl.res.b <- tkbutton(gui.anovapl.res,text="Close")
          tkconfigure(gui.anovapl.res.t,yscrollcommand=function(...) tkset(gui.anovapl.res.s,...))
          tkpack(gui.anovapl.res.s,fill="y",side="right")
          tkpack(gui.anovapl.res.t,fill="both",expand=TRUE)
          tkpack(gui.anovapl.res.b) 
          
          tt <- textConnection("x","w")
          sink(tt)
          print(obj.stats[[runindex]])
          sink()
          tkinsert(gui.anovapl.res.t,"end",paste(x,"\n",collapse=""))
          tkconfigure(gui.anovapl.res.b,command=function(...) tkdestroy(gui.anovapl.res))
          tkwait.window(gui.anovapl.res)
          }
      }
     
     assign("obj.stats",obj.stats,nenv)
  }


tkconfigure(gui.42ba,command=anovapl)






# ANOVA INT
anovaint.args <- function(){   
   nenvtemp <- new.env();
   assign("tmp",list(NULL),nenvtemp)
   gui.anovaint <- tktoplevel()
   tktitle(gui.anovaint ) <- "Options for ANOVS int"
 
   N <- tclVar(); tclvalue(N) <- 10    
   gui.anovaint.l1 <- tklabel(gui.anovaint,text="Number of intervals:")
   gui.anovaint.e1 <- tkentry(gui.anovaint,width=5)
   tkinsert(gui.anovaint.e1,"end",tclvalue(N))
   
  
   gui.anovaint.b2a <- tkbutton(gui.anovaint,text="OK",width=5)
   gui.anovaint.b2b <- tkbutton(gui.anovaint,text="Cancel",width=5)
   
  
   tkgrid(gui.anovaint.l1,gui.anovaint.e1)
   
   tkgrid(gui.anovaint.b2a,gui.anovaint.b2b)
   
   desguianovaint <- function(){
    tkdestroy(gui.anovaint)
   } 
   tkconfigure(gui.anovaint.b2b,command=desguianovaint)
   returnanovaint <- function(){
     assign("tmp",list(N=tclvalue(tkget(gui.anovaint.e1))),nenvtemp)
     tkdestroy(gui.anovaint)  
   } 
   tkconfigure(gui.anovaint.b2a,command=returnanovaint)
   tkwait.window(gui.anovaint)  
   return(get("tmp",nenvtemp))
 

}

anovaintensity <- function(){
     if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }
     l <- anovaint.args()

     if (!is.null(l[[1]])){
      index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
      if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
          }
      obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
       runindex <- runindex + 1;
       obj.stats[[runindex]]  <- anovaint(get("obj",nenv),index[runindex],N = as.integer(l$N)) 
                               
      

      if (tclvalue(statsvisu)=="1"){
          gui.anovaint.res <- tktoplevel()
          tktitle(gui.anovaint.res) <- paste("Results of ANOVA for array",index[[runindex]])
          gui.anovaint.res.l <- tklabel(gui.anovaint.res,text="Summary of ANOVA model:")
          gui.anovaint.res.t <- tktext(gui.anovaint.res)
          gui.anovaint.res.s <- tkscrollbar(gui.anovaint.res,command=function(...) tkyview(gui.anovaint.res.t,...))
          gui.anovaint.res.b <- tkbutton(gui.anovaint.res,text="Close")
          tkconfigure(gui.anovaint.res.t,yscrollcommand=function(...) tkset(gui.anovaint.res.s,...))
          tkpack(gui.anovaint.res.s,fill="y",side="right")
          tkpack(gui.anovaint.res.t,fill="both",expand=TRUE)
          tkpack(gui.anovaint.res.b) 
          
          tt <- textConnection("x","w")
          sink(tt)
          #print(get("obj.stats",nenv))
          print(obj.stats[[runindex]])
          sink()
          tkinsert(gui.anovaint.res.t,"end",paste(x,"\n",collapse=""))
          tkconfigure(gui.anovaint.res.b,command=function(...) tkdestroy(gui.anovaint.res))
          tkwait.window(gui.anovaint.res)
          }
      }
     
     assign("obj.stats",obj.stats,nenv)
  }
}

tkconfigure(gui.43ba,command=anovaintensity)





# ANOVA SPA
anovaspa.args <- function(){   
   nenvtemp <- new.env();
   assign("tmp",list(NULL),nenvtemp)
   gui.anovaspa <- tktoplevel()
   tktitle(gui.anovaspa ) <- "Options for ANOVS spa"
 
   xN <- tclVar(); tclvalue(xN) <- 5    
   gui.anovaspa.l1 <- tklabel(gui.anovaspa,text="Number of intervals in X-direction:")
   gui.anovaspa.e1 <- tkentry(gui.anovaspa,width=5)
   tkinsert(gui.anovaspa.e1,"end",tclvalue(xN))
   yN <- tclVar(); tclvalue(yN) <- 5    
   gui.anovaspa.l2 <- tklabel(gui.anovaspa,text="Number of intervals in Y-direction:")
   gui.anovaspa.e2 <- tkentry(gui.anovaspa,width=5)
   tkinsert(gui.anovaspa.e2,"end",tclvalue(yN))
   
     
   gui.anovaspa.b2a <- tkbutton(gui.anovaspa,text="OK",width=5)
   gui.anovaspa.b2b <- tkbutton(gui.anovaspa,text="Cancel",width=5)
   
  
   tkgrid(gui.anovaspa.l1,gui.anovaspa.e1)
   tkgrid(gui.anovaspa.l2,gui.anovaspa.e2)

   tkgrid(gui.anovaspa.b2a,gui.anovaspa.b2b)
   
   tkconfigure(gui.anovaspa.b2b,command=function(...) tkdestroy(gui.anovaspa))
   returnanovaspa <- function(){
     assign("tmp",list(xN=tclvalue(tkget(gui.anovaspa.e1)),yN=tclvalue(tkget(gui.anovaspa.e2))),nenvtemp)
     tkdestroy(gui.anovaspa)  
   } 
   tkconfigure(gui.anovaspa.b2a,command=returnanovaspa)
   tkwait.window(gui.anovaspa)  
   return(get("tmp",nenvtemp))
 

}

anovaspa <- function(){
      if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }
     l <- anovaspa.args()

     if (!is.null(l[[1]])){
       index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
      if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
          }
      obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
       runindex <- runindex + 1;
 
  
      if (tclvalue(statsvisu)!="1"){
       obj.stats[[runindex]]  <- anovaspatial(get("obj",nenv),index[runindex],xN = as.integer(l$xN),yN=as.integer(l$yN)) 
                               
       assign("obj.stats",obj.stats,nenv)
      }
      if (tclvalue(statsvisu)=="1"){
          gui.anovaspa.res <- tktoplevel()
          tktitle(gui.anovaspa.res) <- paste("Results of ANOVA spatial for array",index[runindex])
          gui.anovaspa.res.l <- tklabel(gui.anovaspa.res,text="Summary of ANOVA model:")
          gui.anovaspa.res.t <- tktext(gui.anovaspa.res)
          gui.anovaspa.res.s <- tkscrollbar(gui.anovaspa.res,command=function(...) tkyview(gui.anovaspa.res.t,...))
          gui.anovaspa.res.b <- tkbutton(gui.anovaspa.res,text="Close")
          tkconfigure(gui.anovaspa.res.t,yscrollcommand=function(...) tkset(gui.anovaspa.res.s,...))
          tkpack(gui.anovaspa.res.s,fill="y",side="right")
          tkpack(gui.anovaspa.res.t,fill="both",expand=TRUE)
          tkpack(gui.anovaspa.res.b) 
          
          tt <- textConnection("x","w")
          sink(tt)

          print(obj.stats[[runindex]]  <- anovaspatial(get("obj",nenv),index[runindex],xN = as.integer(l$xN),yN=as.integer(l$yN),visu=TRUE))     
          sink()
         
          tkinsert(gui.anovaspa.res.t,"end",paste(x,"\n",collapse=""))
          tkconfigure(gui.anovaspa.res.b,command=function(...) tkdestroy(gui.anovaspa.res))
          tkwait.window(gui.anovaspa.res)   
        }
      }
 assign("obj.stats",obj.stats,nenv)   
}
}
     


tkconfigure(gui.44ba,command=anovaspa)



################################################################
# FDR INT
fdr.int.args <- function(){   
   nenvtemp <- new.env();
   assign("tmp",list(NULL),nenvtemp)
   gui.fdr.int <- tktoplevel()
   tktitle(gui.fdr.int ) <- "Options for fdr.int"
   windowvar <- tclVar(); tclvalue(windowvar) <- 50
   gui.fdr.int.l1 <- tklabel(gui.fdr.int,text="Size of sliding window (2s+1):")
   gui.fdr.int.e1 <- tkentry(gui.fdr.int,width=5)
   tkinsert(gui.fdr.int.e1,"end",tclvalue(windowvar))
   parvar <- tclVar(); tclvalue(parvar) <- 100
   
   gui.fdr.int.l2 <- tklabel(gui.fdr.int,text="Number of permutations:")
   gui.fdr.int.e2 <- tkentry(gui.fdr.int,width=5)
   tkinsert(gui.fdr.int.e2,"end",tclvalue(parvar))
   gui.fdr.int.l3 <- tklabel(gui.fdr.int,text="Averaging method:")
   avmethod <- tclVar(); tclvalue(avmethod) <- "median" 
   gui.fdr.int.r3a <- tkradiobutton(gui.fdr.int,text="Mean",
                                     variable=avmethod,value="mean")
   gui.fdr.int.r3b <- tkradiobutton(gui.fdr.int,text="Median",
                                     variable=avmethod,value="median")
  
   gui.fdr.int.b5 <- tkbutton(gui.fdr.int,text="OK",width=5)
   gui.fdr.int.b5b <- tkbutton(gui.fdr.int,text="Cancel",width=5)
   
  
   tkgrid(gui.fdr.int.l1,gui.fdr.int.e1)
   tkgrid.configure(gui.fdr.int.e1,column=2,columnspan=1)
   tkgrid.configure(gui.fdr.int.l1,columnspan=2,sticky="w")
   tkgrid(gui.fdr.int.l2,gui.fdr.int.e2)
   tkgrid.configure(gui.fdr.int.l2,columnspan=2,sticky="w")
   tkgrid.configure(gui.fdr.int.e2,column=2,columnspan=1)
   tkgrid(gui.fdr.int.l3,gui.fdr.int.r3a,gui.fdr.int.r3b)
   tkgrid.configure(gui.fdr.int.l3,sticky="e")
   tkgrid.configure(gui.fdr.int.l3,sticky="w")  
   tkgrid(gui.fdr.int.b5,gui.fdr.int.b5b)
   tkgrid(gui.fdr.int.b5b,column=2)
   desguifdrint <- function(){
    tkdestroy(gui.fdr.int)
   } 
   tkconfigure(gui.fdr.int.b5b,command=desguifdrint)
   returnfdrint <- function(){
     assign("tmp",list(delta=tclvalue(tkget(gui.fdr.int.e1)),pn=tclvalue(tkget(gui.fdr.int.e2)),
               avm=tclvalue(avmethod)),nenvtemp)
     tkdestroy(gui.fdr.int)  
   } 
   tkconfigure(gui.fdr.int.b5,command=returnfdrint)
   tkwait.window(gui.fdr.int)  
   return(get("tmp",nenvtemp))
 

}

fdr.intensity <- function(){
       if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             } 
     l <- fdr.int.args()

     if (!is.null(l[[1]])){
    
      index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
      if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
          }
      obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
       runindex <- runindex + 1;
       obj.stats[[runindex]]   <- fdr.int(maA(get("obj",nenv))[,index[runindex]],maM(get("obj",nenv))[,index[runindex]],
                                 delta = as.integer(l$delta), N = as.integer(l$pn), 
                                 av = l$avm)
      
      if (tclvalue(statsvisu)=="1"){
         sigint.plot(maA(get("obj",nenv))[,index[runindex]],maM(get("obj",nenv))[,index[runindex]],obj.stats[[runindex]]$FDRp,obj.stats[[runindex]]$FDRn,c(-5,-5))
         if (i!=index[length(index)]){
         tkmessageBox(message="Next?")
        }
      }
   }
    assign("obj.stats",obj.stats,nenv)
 }  
}

tkconfigure(gui.43bb,command=fdr.intensity)
###########################################################################
# FDR SPA
fdr.spa.args <- function(){   
   nenvtemp <- new.env();
   assign("tmp",list(NULL),nenvtemp)
   gui.fdr.spa <- tktoplevel()
   tktitle(gui.fdr.spa ) <- "Options for fdr.spa"
   windowvar <- tclVar(); tclvalue(windowvar) <- 2
   gui.fdr.spa.l1 <- tklabel(gui.fdr.spa,text="Size of sliding window (2s+1)x(2s+1):")
   gui.fdr.spa.e1 <- tkentry(gui.fdr.spa,width=5)
   tkinsert(gui.fdr.spa.e1,"end",tclvalue(windowvar))
   parvar <- tclVar(); tclvalue(parvar) <- 100
   
   gui.fdr.spa.l2 <- tklabel(gui.fdr.spa,text="Number of permutations:")
   gui.fdr.spa.e2 <- tkentry(gui.fdr.spa,width=5)
   tkinsert(gui.fdr.spa.e2,"end",tclvalue(parvar))
   gui.fdr.spa.l3 <- tklabel(gui.fdr.spa,text="Averaging method:")
   avmethod <- tclVar(); tclvalue(avmethod) <- "median" 
   gui.fdr.spa.r3a <- tkradiobutton(gui.fdr.spa,text="Mean",
                                     variable=avmethod,value="mean")
   gui.fdr.spa.r3b <- tkradiobutton(gui.fdr.spa,text="Median",
                                     variable=avmethod,value="median")
   gui.fdr.spa.l4 <- tklabel(gui.fdr.spa,text="Edge value included:")
   edgetr <- tclVar(); tclvalue(edgetr) <- "TRUE"
   gui.fdr.spa.r4a <- tkradiobutton(gui.fdr.spa,text="Yes",
                                     variable=edgetr,value="TRUE")
   gui.fdr.spa.r4b <- tkradiobutton(gui.fdr.spa,text="No",
                                     variable=edgetr,value="FALSE")
   gui.fdr.spa.b5 <- tkbutton(gui.fdr.spa,text="OK",width=5)
   gui.fdr.spa.b5b <- tkbutton(gui.fdr.spa,text="Cancel",width=5)
  
   tkgrid(gui.fdr.spa.l1,gui.fdr.spa.e1)
   tkgrid.configure(gui.fdr.spa.e1,column=2,columnspan=1)
   tkgrid.configure(gui.fdr.spa.l1,columnspan=2,sticky="w")
   tkgrid(gui.fdr.spa.l2,gui.fdr.spa.e2)
   tkgrid.configure(gui.fdr.spa.l2,columnspan=2,sticky="w")
   tkgrid.configure(gui.fdr.spa.e2,column=2,columnspan=1)
   tkgrid(gui.fdr.spa.l3,gui.fdr.spa.r3a,gui.fdr.spa.r3b)
   tkgrid.configure(gui.fdr.spa.l3,sticky="e")
   tkgrid.configure(gui.fdr.spa.l3,sticky="w")
   tkgrid(gui.fdr.spa.l4,gui.fdr.spa.r4a,gui.fdr.spa.r4b)
   tkgrid.configure(gui.fdr.spa.l4,sticky="w")
   tkgrid(gui.fdr.spa.b5,gui.fdr.spa.b5b)
   tkgrid(gui.fdr.spa.b5b,column=2)
   desguifdrspa <- function(){
     tkdestroy(gui.fdr.spa)  
   } 
   tkconfigure(gui.fdr.spa.b5b,command=desguifdrspa)
   returnfdrspa <- function(){
     assign("tmp",list(delta=tclvalue(tkget(gui.fdr.spa.e1)),pn=tclvalue(tkget(gui.fdr.spa.e2)),
               avm=tclvalue(avmethod),edg=tclvalue(edgetr)),nenvtemp)
     tkdestroy(gui.fdr.spa)  
   }     
   tkconfigure(gui.fdr.spa.b5,command=returnfdrspa)
   tkwait.window(gui.fdr.spa)
   return(get("tmp",nenvtemp))
}     
  


fdr.spa <- function(){
      if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }
     l <- fdr.spa.args();
     if (!is.null(l[[1]])){ 
       index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
        if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }
        obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
         runindex <- runindex + 1;
      
  
     X <- v2m(maM(get("obj",nenv))[,index[runindex]],Ngc=maNgc(get("obj",nenv)),
                   Ngr=maNgr(get("obj",nenv)),Nsc=maNsc(get("obj",nenv)),Nsr=maNsr(get("obj",nenv))) 
     obj.stats[[runindex]]  <- fdr.spatial(X, delta = as.integer(l$delta), N = as.integer(l$pn), 
                                 av = l$avm, edgeNA = as.logical(l$edg))
       assign("obj.stats",obj.stats,nenv)
     if (tclvalue(statsvisu)=="1"){
      sigxy.plot(obj.stats[[runindex]]$FDRp,obj.stats[[runindex]]$FDRn,color.lim=c(-5,5),main="FDR")
       if (i!=index[length(index)]){
         tkmessageBox(message="Next?")
        }
     }
   }
    assign("obj.stats",obj.stats,nenv)
 }  
}

tkconfigure(gui.44bb,command=fdr.spa)
######################################################################################
# P INT
p.int.args <- function(){   
   nenvtemp <- new.env();
   assign("tmp",list(NULL),nenvtemp)
   gui.p.int <- tktoplevel()
   tktitle(gui.p.int) <- "Options for p.int"
   windowvar <- tclVar(); tclvalue(windowvar) <- 50
   gui.p.int.l1 <- tklabel(gui.p.int,text="Size of sliding window (2s+1):")
   gui.p.int.e1 <- tkentry(gui.p.int,width=5)
   tkinsert(gui.p.int.e1,"end",tclvalue(windowvar))
   parvar <- tclVar(); tclvalue(parvar) <- dim(get("obj",nenv))[1]*100
   
   gui.p.int.l2 <- tklabel(gui.p.int,text="Number of random samples:")
   gui.p.int.e2 <- tkentry(gui.p.int,width=8)

   tkinsert(gui.p.int.e2,"end",tclvalue(parvar))
   gui.p.int.l3 <- tklabel(gui.p.int,text="Averaging method:")
   avmethod <- tclVar(); tclvalue(avmethod) <- "median" 
   gui.p.int.r3a <- tkradiobutton(gui.p.int,text="Mean",
                                     variable=avmethod,value="mean")
   gui.p.int.r3b <- tkradiobutton(gui.p.int,text="Median",
                                     variable=avmethod,value="median")
   gui.p.int.l4 <- tklabel(gui.p.int,text="P-value adjustment:")
   padjust <- tclVar(); tclvalue(padjust) <- "none"
   gui.p.int.r4a <- tkradiobutton(gui.p.int,text="None",
                                     variable=padjust,value="none")
   gui.p.int.r4b <- tkradiobutton(gui.p.int,text="Bonferroni",
                                     variable=padjust,value="bonferroni")
   gui.p.int.r4c <- tkradiobutton(gui.p.int,text="Holm",
                                     variable=padjust,value="holm")
   gui.p.int.r4d <- tkradiobutton(gui.p.int,text="Hochberg",
                                     variable=padjust,value="hochberg")
    gui.p.int.r4e <- tkradiobutton(gui.p.int,text="Hommel",
                                     variable=padjust,value="hommel")
   gui.p.int.r4f <- tkradiobutton(gui.p.int,text="fdr",
                                     variable=padjust,value="fdr")
   
   gui.p.int.b5 <- tkbutton(gui.p.int,text="OK",width=5)
   gui.p.int.b5b <- tkbutton(gui.p.int,text="Cancel",width=5)
   
   tkgrid(gui.p.int.l1,gui.p.int.e1)
   tkgrid.configure(gui.p.int.e1,column=2,columnspan=1,sticky="w")
   tkgrid.configure(gui.p.int.l1,columnspan=2,sticky="w")
   tkgrid(gui.p.int.l2,gui.p.int.e2)
   tkgrid.configure(gui.p.int.l2,columnspan=2,sticky="w")
   tkgrid.configure(gui.p.int.e2,column=2,columnspan=1,sticky="w")
   tkgrid(gui.p.int.l3,gui.p.int.r3a,gui.p.int.r3b)
   tkgrid.configure(gui.p.int.l3,sticky="e")
   tkgrid.configure(gui.p.int.l3,sticky="w")
   tkgrid(gui.p.int.l4)
   tkgrid.configure(gui.p.int.l4,sticky="w")

   tkgrid(gui.p.int.r4a,gui.p.int.r4b,gui.p.int.r4c)
  
   tkgrid.configure(gui.p.int.r4a,sticky="w")
   tkgrid.configure(gui.p.int.r4b,sticky="w")
   tkgrid.configure(gui.p.int.r4c,sticky="w")
   tkgrid(gui.p.int.r4d,gui.p.int.r4e,gui.p.int.r4f)
   tkgrid.configure(gui.p.int.r4d,sticky="w")
   tkgrid.configure(gui.p.int.r4e,sticky="w")
   tkgrid.configure(gui.p.int.r4f,sticky="w")
   tkgrid(gui.p.int.b5,gui.p.int.b5b)
   tkgrid(gui.p.int.b5b,column=2)
   desguipint <- function(){
      tkdestroy(gui.p.int)
   } 
   tkconfigure(gui.p.int.b5b,command=desguipint)
   returnpint <- function(){
       assign("tmp",list(delta=tclvalue(tkget(gui.p.int.e1)),pn=tclvalue(tkget(gui.p.int.e2)),
               avm=tclvalue(avmethod),p.adjust=tclvalue(padjust)),nenvtemp)
       tkdestroy(gui.p.int)
   } 
   tkconfigure(gui.p.int.b5,command=returnpint)
   tkwait.window(gui.p.int)
   return(get("tmp",nenvtemp))
  

}

p.intensity <- function(){
      if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }
     l <- p.int.args();
     if (!is.null(l[[1]])){
       index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
        if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }
        obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
         runindex <- runindex + 1;
  
     obj.stats[[runindex]]  <- p.int(maA(get("obj",nenv))[,index[runindex]],maM(get("obj",nenv))[,index[runindex]],
                                 delta = as.integer(l$delta), N = as.integer(l$pn), 
                                 av = l$avm,p.adjust.method=l$p.adjust)
       
     if (tclvalue(statsvisu)=="1"){
      sigint.plot(maA(get("obj",nenv))[,index[runindex]],maM(get("obj",nenv))[,index[runindex]],obj.stats[[runindex]]$Pp,obj.stats[[runindex]]$Pn,
                  c(-5,-5))
      
   if (i!=index[length(index)]){
         tkmessageBox(message="Next?")
        }
    }
   }
       assign("obj.stats",obj.stats,nenv)
}
}

tkconfigure(gui.43bc,command=p.intensity)
#######################################################################################
# P SPA
p.spa.args <- function(){  
   nenvtemp <- new.env();
   assign("tmp",list(NULL),nenvtemp) 
   gui.p.spa <- tktoplevel()
   tktitle(gui.p.spa) <- "Options for p.spa"
   windowvar <- tclVar(); tclvalue(windowvar) <- 2
   gui.p.spa.l1 <- tklabel(gui.p.spa,text="Size of sliding window (2s+1)x(2s+1):")
   gui.p.spa.e1 <- tkentry(gui.p.spa,width=5)
   tkinsert(gui.p.spa.e1,"end",tclvalue(windowvar))
   parvar <- tclVar(); tclvalue(parvar) <- dim(get("obj",nenv))[1]*100
   
   gui.p.spa.l2 <- tklabel(gui.p.spa,text="Number of random samples:")
   gui.p.spa.e2 <- tkentry(gui.p.spa,width=8)

   tkinsert(gui.p.spa.e2,"end",tclvalue(parvar))
   gui.p.spa.l3 <- tklabel(gui.p.spa,text="Averaging method:")
   avmethod <- tclVar(); tclvalue(avmethod) <- "median" 
   gui.p.spa.r3a <- tkradiobutton(gui.p.spa,text="Mean",
                                     variable=avmethod,value="mean")
   gui.p.spa.r3b <- tkradiobutton(gui.p.spa,text="Median",
                                     variable=avmethod,value="median")
   gui.p.spa.l4 <- tklabel(gui.p.spa,text="P-value adjustment:")
   padjust <- tclVar(); tclvalue(padjust) <- "none"
   gui.p.spa.r4a <- tkradiobutton(gui.p.spa,text="None",
                                     variable=padjust,value="none")
   gui.p.spa.r4b <- tkradiobutton(gui.p.spa,text="Bonferroni",
                                     variable=padjust,value="bonferroni")
   gui.p.spa.r4c <- tkradiobutton(gui.p.spa,text="Holm",
                                     variable=padjust,value="holm")
   gui.p.spa.r4d <- tkradiobutton(gui.p.spa,text="Hochberg",
                                     variable=padjust,value="hochberg")
    gui.p.spa.r4e <- tkradiobutton(gui.p.spa,text="Hommel",
                                     variable=padjust,value="hommel")
   gui.p.spa.r4f <- tkradiobutton(gui.p.spa,text="fdr",
                                     variable=padjust,value="fdr")
   
   gui.p.spa.b5 <- tkbutton(gui.p.spa,text="OK",width=5)
   gui.p.spa.b5b <- tkbutton(gui.p.spa,text="Cancel",width=5)
   
   tkgrid(gui.p.spa.l1,gui.p.spa.e1)
   tkgrid.configure(gui.p.spa.e1,column=2,columnspan=1,sticky="w")
   tkgrid.configure(gui.p.spa.l1,columnspan=2,sticky="w")
   tkgrid(gui.p.spa.l2,gui.p.spa.e2)
   tkgrid.configure(gui.p.spa.l2,columnspan=2,sticky="w")
   tkgrid.configure(gui.p.spa.e2,column=2,columnspan=1,sticky="w")
   tkgrid(gui.p.spa.l3,gui.p.spa.r3a,gui.p.spa.r3b)
   tkgrid.configure(gui.p.spa.l3,sticky="e")
   tkgrid.configure(gui.p.spa.l3,sticky="w")
   tkgrid(gui.p.spa.l4)
   tkgrid.configure(gui.p.spa.l4,sticky="w")

   tkgrid(gui.p.spa.r4a,gui.p.spa.r4b,gui.p.spa.r4c)
  
   tkgrid.configure(gui.p.spa.r4a,sticky="w")
   tkgrid.configure(gui.p.spa.r4b,sticky="w")
   tkgrid.configure(gui.p.spa.r4c,sticky="w")
   tkgrid(gui.p.spa.r4d,gui.p.spa.r4e,gui.p.spa.r4f)
   tkgrid.configure(gui.p.spa.r4d,sticky="w")
   tkgrid.configure(gui.p.spa.r4e,sticky="w")
   tkgrid.configure(gui.p.spa.r4f,sticky="w")
   tkgrid(gui.p.spa.b5,gui.p.spa.b5b)
   tkgrid(gui.p.spa.b5b,column=2)
   desguipspa <- function(){
    tkdestroy(gui.p.spa)
   } 
   tkconfigure(gui.p.spa.b5b,command=desguipspa)
   returnpspa <- function(){
     assign("tmp",list(delta=tclvalue(tkget(gui.p.spa.e1)),pn=tclvalue(tkget(gui.p.spa.e2)),
               avm=tclvalue(avmethod),p.adjust=tclvalue(padjust)),nenvtemp)
     tkdestroy(gui.p.spa)  
   } 
   tkconfigure(gui.p.spa.b5,command=returnpspa)
   tkwait.window(gui.p.spa)
   return(get("tmp",nenvtemp))
   
 }



p.spa <- function(){
      if (!exists("obj",nenv)){
                  tkmessageBox(message="No marray has been loaded.")
                  return()
             }
     l <- p.spa.args();
     if (!is.null(l[[1]])){
   
        index <- as.integer(strsplit(tclvalue(tkget(gui.42e))," ")[[1]])[!is.na(as.real(strsplit(tclvalue(tkget(gui.42e))," ")[[1]]))] 
    
        if (length(index)==0){
              index <- 1:dim(maM(get("obj",nenv)))[[2]] 
            }
        obj.stats <- list(NULL);runindex <- 0;
      for (i in index){
         runindex <- runindex + 1;
  
    
     X <- v2m(maM(get("obj",nenv))[,index[runindex]],Ngc=maNgc(get("obj",nenv)),
                   Ngr=maNgr(get("obj",nenv)),Nsc=maNsc(get("obj",nenv)),Nsr=maNsr(get("obj",nenv))) 
     obj.stats[[runindex]]   <- p.spatial(X, delta = as.integer(l$delta), N = as.integer(l$pn), 
                                 av = l$avm, p.adjust.method = l$p.adjust)
      
     if (tclvalue(statsvisu)=="1"){
      sigxy.plot(obj.stats[[runindex]]$Pp,obj.stats[[runindex]]$Pn,color.lim=c(-5,5),main="P-value")
         if (i!=index[length(index)]){
         tkmessageBox(message="Next?")
        }
     }
   }
  assign("obj.stats",obj.stats,nenv)    
}
}
tkconfigure(gui.44bc,command=p.spa)
#############################################################################
# EXPORTING AND SAVING STATISTICS RESULTS

exportstat <- function(){
             if (!exists("obj.stats",nenv)){
                  tkmessageBox(message="No statistical test has been applied yet.")
                  return()
             }
            gui.export <- tktoplevel()
            tktitle(gui.export) <- "Choose"
            gui.export.l <- tklabel(gui.export,text="Choose name for object:")
            tmp <- tclVar(); tclvalue(tmp) <- "obj.stats"; 
            gui.export.e <- tkentry(gui.export)
            tkinsert(gui.export.e,"end",tclvalue(tmp))
            gui.export.b <- tkbutton(gui.export,text="Export to global environment")
            tkgrid(gui.export.l);tkgrid(gui.export.e);tkgrid(gui.export.b)
            exportobj <- function(){ 
                      assign(as.character(tclvalue(tkget(gui.export.e))),get("obj.stats",nenv),envir=globalenv())
                      tkdestroy(gui.export)
             }
            tkconfigure(gui.export.b,command=exportobj)
 
}
tkconfigure(gui.46bb,command=exportstat)

savestats <- function(){
             if (!exists("obj.stats",nenv)){
                  tkmessageBox(message="No statistical test has been applied yet.")
                  return()
             } 
             obj.stats <- get("obj.stats",nenv) 
             f <- tclvalue(tkgetSaveFile())
             if (f !=""){ 
             save(obj.stats,file=f)
             }
            }
tkconfigure(gui.46b,command=savestats)
###################################################

desguiolin <- function(){ 
           tkdestroy(gui)
      }
tkconfigure(gui.51b,command=desguiolin)

}
