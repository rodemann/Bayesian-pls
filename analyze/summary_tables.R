library(vtable)


sumtable(LifeCycleSavings)
vartable <- vtable(LifeCycleSavings,out='return')

#I can easily \input this into my LaTeX doc:
# vt(mtcars,out='latex')#,file='mytable1.tex')
# vt(sonar,out='latex')#,file='mytable1.tex')
vt(data_frame,out='latex')#,file='mytable1.tex')

data(sonar)
