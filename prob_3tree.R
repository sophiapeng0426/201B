tree.heart=tree(AHD~.,data=Heart)
#summary(tree.heart)
plot(tree.heart)
text(tree.heart,pretty=0)


##tree method
tree.pred=predict(tree.heart,test.data,type="class")
table(tree.pred)
cv.heart=cv.tree(tree.heart,FUN=prune.misclass)
plot(cv.heart)
prune.heart=prune.misclass(tree.heart,best=6)
plot(prune.heart)
text(prune.heart,pretty=0)