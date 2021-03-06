SKAT_Null_Model_Get_Includes<-function( obj_omit, obj_pass){

	ID1<-rownames(obj_omit)
	ID2<-rownames(obj_pass)

	d1<-data.frame(ID=ID1)
	d2<-data.frame(ID=ID2, idx=1:length(ID2))

	d3<-merge(d1, d2,by.x="ID", by.y="ID")
	id_include = sort(d3$idx)

	return(id_include)
}
