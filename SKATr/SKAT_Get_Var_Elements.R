SKAT_Get_Var_Elements<-function(m4,p_all,u1,u2){

	temp1<-u1^2 * u2^2


	a1<-sum(m4 * temp1)
	a2<-sum(u1^2) * sum(u2^2) - sum(temp1)
	a3<-sum(u1*u2)^2 - sum(temp1)

	a3<-a3*2


	a1+a2+a3
}
