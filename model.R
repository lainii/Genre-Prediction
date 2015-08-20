##########################################################################################
#Initilization:
# Just run all of the following, it sets up 3 prediction equations as denoted
# in the comments in the section. 
# It takes ~5min to run the mlogit function calls, get a snack
#
#INSTRUCTIONS FOR RUNNING WITH musicTEST
# 1) run all code up until the how to run predictions section
# 2) run the following line of code
#	newDataPC = obtainPC(valid_no_genres);
# 3) run the desired model section as written
# 4) read in the baseline_solution.txt text file
# 4) modify the following code to get the accuracy table (baseline_solution is the column vector with the solutions)
#	table(baseline_solution, p[,7])
# 		
##########################################################################################
#read data
library(mlogit)
musicTrain = read.table("genresTrain.csv", sep=",", header = T);
musicTest = read.table("genresTest.csv", sep=",", header = T);

#training data/ validation data (60%/ 40% of musicTrain)
train=musicTrain[1:7497,];
validation=musicTrain[7498:12495,];
valid_no_genres=validation[,1:191];

#PCA
pca=prcomp(train[,1:191],scale=T,retx=T);
pcs=pca$x[,1:74];
musicPCA=data.frame(pcs,train$GENRE);
names(musicPCA)[75]='GENRE';

#format data

musicData=mlogit.data(musicPCA,varying=NULL,choice='GENRE', shape='wide');

#make function call using 64 pcs ~ 0.93373 cumulative proportion
f1=mFormula(GENRE~0| PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + PC33 + PC34 + PC35 + PC36 + PC37 + PC38 + PC39 + PC40 + PC41 + PC42 + PC43 + PC44 + PC45 + PC46 + PC47 + PC48 + PC49 + PC50 + PC51 + PC52 + PC53 + PC54 + PC55 + PC56 + PC57 + PC58 + PC59 + PC60 + PC61 + PC62 + PC63 + PC64);
model1=mlogit(formula=f1,data=musicData,reflevel='Blues')

#using 19 pcs ~0.70192 cumulative proportion
f2=mFormula(GENRE~0| PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19);
model2=mlogit(formula=f2,data=musicData,reflevel='Blues')

#using 30 pcs ~0.80115 cumulative proportion
f3=mFormula(GENRE~0| PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30);
model3=mlogit(formula=f3,data=musicData,reflevel='Blues')
##########################################################################################
#Set up new data to be predicted--
#	See James' code
#NOTE: function obtainPC requires data with no genre column
#ie. valid_no_genre 
#
##########################################################################################
#portion of James' Code
#NOTE: obtainPC requires data with no genre column
# Global vars & data frames
colmean = vector();
colstdev = vector();

# Obtain PCs for new data
obtainPC = function(newData){
x <- train[,1:(ncol(train)-1)];
x <- as.matrix(x);
data <- newData;
data <- as.matrix(data);
for(i in (1:(ncol(x)))){
colmean[i] <- mean(x[,i]);
colstdev[i] <- sd(x[,i]);
x[,i] <- (x[,i]-mean(x[,i]))/sd(x[,i])};
V = svd(x)$v[,1:64];
for(i in (1:(ncol(data)))){
data[,i] <- (data[,i]-colmean[i])/colstdev[i]};
pcs = data%*%V;
return(pcs);
}

newDataPC = obtainPC(valid_no_genres);
##########################################################################################
#How to run predictions
# 1) equations are set using my code Predictions which was emailed, this is the
# script at the bottom which fixes the text. Make sure to run y=coef(model) first
# 2) run all the code in the section for the desired model
# 3) output is a table p with the predictions in the 7th column
# 4) compare this with the actual outcomes using the table function
##########################################################################################
##########################################################################################
#MODEL 1--64 PCS
##########################################################################################

#Set up prediction equations for model1
N=dim(newDataPC)[1]

y=coef(model1)

logitBlues=rep(0,N);
logitClassical=y[1]+ -1.64042236179218 *newDataPC[, 1 ] + -0.696672189904826 *newDataPC[, 2 ] + 0.6532918523269 *newDataPC[, 3 ] + 1.20163468120152 *newDataPC[, 4 ] + 0.730208126257605 *newDataPC[, 5 ] + -0.714162747363286 *newDataPC[, 6 ] + -0.0720626983411938 *newDataPC[, 7 ] + 1.2726783336037 *newDataPC[, 8 ] + 0.490875410692302 *newDataPC[, 9 ] + -0.52374611109559 *newDataPC[, 10 ] + -0.409203346524904 *newDataPC[, 11 ] + 0.689415251678204 *newDataPC[, 12 ] + 0.910279039750359 *newDataPC[, 13 ] + 0.472884542546778 *newDataPC[, 14 ] + 1.34254799675465 *newDataPC[, 15 ] + -0.180534097981765 *newDataPC[, 16 ] + -0.29022075298941 *newDataPC[, 17 ] + 1.72056361102311 *newDataPC[, 18 ] + 0.00663134909389946 *newDataPC[, 19 ] + -0.0682844171613815 *newDataPC[, 20 ] + 2.14332987364415 *newDataPC[, 21 ] + 1.05125368289887 *newDataPC[, 22 ] + -1.16666051418045 *newDataPC[, 23 ] + -0.065578576742366 *newDataPC[, 24 ] + -1.53987621396721 *newDataPC[, 25 ] + 1.16182922254118 *newDataPC[, 26 ] + 0.432040556634952 *newDataPC[, 27 ] + -0.620051759296483 *newDataPC[, 28 ] + -0.446053941155451 *newDataPC[, 29 ] + 0.583447484911005 *newDataPC[, 30 ] + -0.88368920929002 *newDataPC[, 31 ] + 1.48242896956181 *newDataPC[, 32 ] + -0.907593790195603 *newDataPC[, 33 ] + -0.424238781538158 *newDataPC[, 34 ] + 1.3000306111189 *newDataPC[, 35 ] + 0.668058987178279 *newDataPC[, 36 ] + -0.523452830789163 *newDataPC[, 37 ] + 0.831046561438168 *newDataPC[, 38 ] + -0.540931695055572 *newDataPC[, 39 ] + -0.120051431700212 *newDataPC[, 40 ] + -0.175612648315817 *newDataPC[, 41 ] + -0.474549654021791 *newDataPC[, 42 ] + -0.0541261839501656 *newDataPC[, 43 ] + -1.62287361220472 *newDataPC[, 44 ] + 0.305564070371121 *newDataPC[, 45 ] + -1.28556292141873 *newDataPC[, 46 ] + 1.36779842840901 *newDataPC[, 47 ] + 0.838349489426862 *newDataPC[, 48 ] + -0.264913715401905 *newDataPC[, 49 ] + 0.163770515391931 *newDataPC[, 50 ] + 0.577777509292456 *newDataPC[, 51 ] + 0.190512454752793 *newDataPC[, 52 ] + -1.9851696223832 *newDataPC[, 53 ] + 1.94750522351504 *newDataPC[, 54 ] + -0.271886593971128 *newDataPC[, 55 ] + 0.37101722941185 *newDataPC[, 56 ] + 0.795300782528143 *newDataPC[, 57 ] + -0.899800615591958 *newDataPC[, 58 ] + -0.883270004064604 *newDataPC[, 59 ] + 0.486486920137559 *newDataPC[, 60 ] + 0.012190451136939 *newDataPC[, 61 ] + 0.631053249300236 *newDataPC[, 62 ] + -0.236098806629917 *newDataPC[, 63 ] + 0.0585324908013928 *newDataPC[, 64 ]
logitJazz=y[2]+ -1.42990299771442 *newDataPC[, 1 ] + -0.534847635949955 *newDataPC[, 2 ] + 1.04513069650555 *newDataPC[, 3 ] + 1.30823781376106 *newDataPC[, 4 ] + 0.330703460122919 *newDataPC[, 5 ] + -0.990482137447854 *newDataPC[, 6 ] + -0.399192529038714 *newDataPC[, 7 ] + 1.41701435922007 *newDataPC[, 8 ] + 0.401439453088922 *newDataPC[, 9 ] + -0.292623846479159 *newDataPC[, 10 ] + -0.740395492228322 *newDataPC[, 11 ] + 0.682316862118455 *newDataPC[, 12 ] + 1.06328515648118 *newDataPC[, 13 ] + 0.396001800430495 *newDataPC[, 14 ] + 1.56508927573557 *newDataPC[, 15 ] + -0.226777200997504 *newDataPC[, 16 ] + -0.380077887308412 *newDataPC[, 17 ] + 1.90978694651521 *newDataPC[, 18 ] + -0.0522650165291462 *newDataPC[, 19 ] + 0.0772287458987415 *newDataPC[, 20 ] + 2.49796326050004 *newDataPC[, 21 ] + 1.4911925135392 *newDataPC[, 22 ] + -1.11109536419996 *newDataPC[, 23 ] + 0.0415409330034016 *newDataPC[, 24 ] + -1.97556456683982 *newDataPC[, 25 ] + 0.73337310147545 *newDataPC[, 26 ] + 0.271672682954056 *newDataPC[, 27 ] + -0.945560359651828 *newDataPC[, 28 ] + -0.736250252225962 *newDataPC[, 29 ] + 0.945130117826938 *newDataPC[, 30 ] + -0.991722466268555 *newDataPC[, 31 ] + 2.2319643201825 *newDataPC[, 32 ] + -1.24292964607862 *newDataPC[, 33 ] + -0.2791162521652 *newDataPC[, 34 ] + 1.74601632695013 *newDataPC[, 35 ] + 0.997068573678295 *newDataPC[, 36 ] + -0.802701383815843 *newDataPC[, 37 ] + 0.837161200086362 *newDataPC[, 38 ] + -0.975061129630334 *newDataPC[, 39 ] + 0.00608819887125431 *newDataPC[, 40 ] + 0.0603268925297415 *newDataPC[, 41 ] + -0.33697632833859 *newDataPC[, 42 ] + -0.165090010119403 *newDataPC[, 43 ] + -1.90848349222139 *newDataPC[, 44 ] + 0.0190748356201598 *newDataPC[, 45 ] + -1.35937570289574 *newDataPC[, 46 ] + 0.692480082296412 *newDataPC[, 47 ] + 1.25188974395729 *newDataPC[, 48 ] + -0.308261692665242 *newDataPC[, 49 ] + 0.0794401653049707 *newDataPC[, 50 ] + 0.119591117287529 *newDataPC[, 51 ] + 0.10254076460934 *newDataPC[, 52 ] + -2.29454079046634 *newDataPC[, 53 ] + 2.8023295810634 *newDataPC[, 54 ] + -0.829491469615183 *newDataPC[, 55 ] + 0.236536487424344 *newDataPC[, 56 ] + 0.819861451055069 *newDataPC[, 57 ] + -0.896646528171413 *newDataPC[, 58 ] + -0.609459388014533 *newDataPC[, 59 ] + 1.00348330990273 *newDataPC[, 60 ] + -0.020550680003305 *newDataPC[, 61 ] + 0.905167649142941 *newDataPC[, 62 ] + -0.124248457347662 *newDataPC[, 63 ] + -0.247646826237492 *newDataPC[, 64 ]
logitMetal=y[3]+ 0.811014156306797 *newDataPC[, 1 ] + -0.471741374103073 *newDataPC[, 2 ] + 0.139222436919709 *newDataPC[, 3 ] + 0.165905925816348 *newDataPC[, 4 ] + -0.165768180310256 *newDataPC[, 5 ] + 0.248653305697997 *newDataPC[, 6 ] + 0.496370045019022 *newDataPC[, 7 ] + 0.906899033383559 *newDataPC[, 8 ] + -1.12258469609349 *newDataPC[, 9 ] + -0.315761840113549 *newDataPC[, 10 ] + -0.1326524958188 *newDataPC[, 11 ] + 0.492488203974874 *newDataPC[, 12 ] + -0.534033687548437 *newDataPC[, 13 ] + -0.588997724844586 *newDataPC[, 14 ] + -0.745836581619038 *newDataPC[, 15 ] + -0.478089759509576 *newDataPC[, 16 ] + 0.238152423918255 *newDataPC[, 17 ] + -0.158346316454927 *newDataPC[, 18 ] + -0.00918773104911346 *newDataPC[, 19 ] + 0.834774350881677 *newDataPC[, 20 ] + 0.126015634168711 *newDataPC[, 21 ] + 1.06306365046461 *newDataPC[, 22 ] + -0.234120086112955 *newDataPC[, 23 ] + -0.188614571269883 *newDataPC[, 24 ] + -0.205259587115186 *newDataPC[, 25 ] + -0.440036179930685 *newDataPC[, 26 ] + 0.112119841155912 *newDataPC[, 27 ] + -0.982304468008407 *newDataPC[, 28 ] + -1.57487450931912 *newDataPC[, 29 ] + -0.176949017964989 *newDataPC[, 30 ] + 0.407980261830554 *newDataPC[, 31 ] + 1.48455302602105 *newDataPC[, 32 ] + -0.103206033992421 *newDataPC[, 33 ] + -1.10629926555688 *newDataPC[, 34 ] + 0.717454053537453 *newDataPC[, 35 ] + 0.0321249035017736 *newDataPC[, 36 ] + -0.102623377332462 *newDataPC[, 37 ] + -0.36193966209514 *newDataPC[, 38 ] + -0.478016470926792 *newDataPC[, 39 ] + 0.0429275240263113 *newDataPC[, 40 ] + -0.747023100989193 *newDataPC[, 41 ] + -0.0494971878712623 *newDataPC[, 42 ] + 0.264477316612298 *newDataPC[, 43 ] + -0.897173676822277 *newDataPC[, 44 ] + 0.587361566324003 *newDataPC[, 45 ] + -0.203487380914096 *newDataPC[, 46 ] + -0.188654865880517 *newDataPC[, 47 ] + 0.713836054105447 *newDataPC[, 48 ] + 0.459456457653245 *newDataPC[, 49 ] + -0.014603915765476 *newDataPC[, 50 ] + 0.0261416954389284 *newDataPC[, 51 ] + -0.420443758887449 *newDataPC[, 52 ] + -0.130334277082467 *newDataPC[, 53 ] + 1.69739758151247 *newDataPC[, 54 ] + -0.476127162612146 *newDataPC[, 55 ] + 0.887017653903403 *newDataPC[, 56 ] + 1.61741831160756 *newDataPC[, 57 ] + -0.514612977718005 *newDataPC[, 58 ] + -0.47872201621517 *newDataPC[, 59 ] + -0.404909192331462 *newDataPC[, 60 ] + 0.104549247247952 *newDataPC[, 61 ] + 0.430520202892027 *newDataPC[, 62 ] + 0.0990524890851979 *newDataPC[, 63 ] + -0.59833943955681 *newDataPC[, 64 ]
logitPop=y[4]+ 0.341218162236595 *newDataPC[, 1 ] + 0.369383317272115 *newDataPC[, 2 ] + 0.386457487809602 *newDataPC[, 3 ] + 0.810017459919939 *newDataPC[, 4 ] + -1.1133489666042 *newDataPC[, 5 ] + -0.230904621083722 *newDataPC[, 6 ] + 0.21986369817249 *newDataPC[, 7 ] + 1.21411715664715 *newDataPC[, 8 ] + -1.03537921090147 *newDataPC[, 9 ] + 0.0746957118620487 *newDataPC[, 10 ] + -0.215555268978393 *newDataPC[, 11 ] + 0.86841354656345 *newDataPC[, 12 ] + -0.168903778321007 *newDataPC[, 13 ] + -0.516710195074928 *newDataPC[, 14 ] + 0.123182770890993 *newDataPC[, 15 ] + -0.0621479793818021 *newDataPC[, 16 ] + 0.145890547113782 *newDataPC[, 17 ] + 0.030178666830645 *newDataPC[, 18 ] + 0.0552362451649969 *newDataPC[, 19 ] + 0.503035802930725 *newDataPC[, 20 ] + -0.248837594439471 *newDataPC[, 21 ] + 1.10554706502774 *newDataPC[, 22 ] + -0.150618016539823 *newDataPC[, 23 ] + -0.277731542163923 *newDataPC[, 24 ] + -0.705324843925529 *newDataPC[, 25 ] + -0.258056607887101 *newDataPC[, 26 ] + 0.101107248050493 *newDataPC[, 27 ] + -0.641612310813408 *newDataPC[, 28 ] + -0.775231241520306 *newDataPC[, 29 ] + 0.0759906143698867 *newDataPC[, 30 ] + -0.353368792834175 *newDataPC[, 31 ] + 0.338852437361148 *newDataPC[, 32 ] + -0.626267742769571 *newDataPC[, 33 ] + -1.62288047910368 *newDataPC[, 34 ] + 0.730863207690815 *newDataPC[, 35 ] + -0.869874056667574 *newDataPC[, 36 ] + -0.212961168473912 *newDataPC[, 37 ] + -0.389760938355744 *newDataPC[, 38 ] + -0.166170799808168 *newDataPC[, 39 ] + -0.153106405307685 *newDataPC[, 40 ] + -0.555200627150456 *newDataPC[, 41 ] + -0.611560210344923 *newDataPC[, 42 ] + 0.392457586784516 *newDataPC[, 43 ] + -0.809953776223769 *newDataPC[, 44 ] + 0.84971284158781 *newDataPC[, 45 ] + 0.145210654667495 *newDataPC[, 46 ] + -0.0129305860465839 *newDataPC[, 47 ] + 0.377577493946245 *newDataPC[, 48 ] + 0.107580118393496 *newDataPC[, 49 ] + -0.336123734885025 *newDataPC[, 50 ] + -0.658529088434504 *newDataPC[, 51 ] + 0.00526047224779497 *newDataPC[, 52 ] + -0.813238527810005 *newDataPC[, 53 ] + 1.01010962742611 *newDataPC[, 54 ] + 0.0738734767191355 *newDataPC[, 55 ] + -0.081536583482363 *newDataPC[, 56 ] + 0.455914282144724 *newDataPC[, 57 ] + 0.332606890244029 *newDataPC[, 58 ] + -0.274105460122629 *newDataPC[, 59 ] + -0.220085891455721 *newDataPC[, 60 ] + 0.240501161006901 *newDataPC[, 61 ] + 0.664885001783064 *newDataPC[, 62 ] + -0.194095349344332 *newDataPC[, 63 ] + 0.594686427207658 *newDataPC[, 64 ]
logitRock=y[5]+ 0.403781469125648 *newDataPC[, 1 ] + -0.197421025043803 *newDataPC[, 2 ] + 0.116578934388367 *newDataPC[, 3 ] + 0.548483481434078 *newDataPC[, 4 ] + -0.652772077137138 *newDataPC[, 5 ] + -0.187219090426173 *newDataPC[, 6 ] + -0.0280072628754949 *newDataPC[, 7 ] + 0.64280832049642 *newDataPC[, 8 ] + -0.594714085391846 *newDataPC[, 9 ] + 0.11883650368642 *newDataPC[, 10 ] + -0.426315930177758 *newDataPC[, 11 ] + 1.0226459771317 *newDataPC[, 12 ] + 0.182781088472833 *newDataPC[, 13 ] + -0.254749385528751 *newDataPC[, 14 ] + 0.448658947178104 *newDataPC[, 15 ] + -0.210363408526309 *newDataPC[, 16 ] + 0.088174654992447 *newDataPC[, 17 ] + 0.661718263661717 *newDataPC[, 18 ] + 0.2130440226833 *newDataPC[, 19 ] + 0.53497962340328 *newDataPC[, 20 ] + 0.15822446810351 *newDataPC[, 21 ] + 0.91314488228141 *newDataPC[, 22 ] + -0.691582408114466 *newDataPC[, 23 ] + -0.567747222215977 *newDataPC[, 24 ] + -0.994929176085764 *newDataPC[, 25 ] + -0.097326467380462 *newDataPC[, 26 ] + -0.432906898762118 *newDataPC[, 27 ] + -0.140394192782542 *newDataPC[, 28 ] + -0.888057349990408 *newDataPC[, 29 ] + 0.426347000352549 *newDataPC[, 30 ] + -0.0120606888171984 *newDataPC[, 31 ] + 0.95620755822222 *newDataPC[, 32 ] + -0.904385557903444 *newDataPC[, 33 ] + -1.30629836810249 *newDataPC[, 34 ] + 0.702115788472557 *newDataPC[, 35 ] + -0.140002393406403 *newDataPC[, 36 ] + -0.281891233164621 *newDataPC[, 37 ] + 0.0435321174405034 *newDataPC[, 38 ] + -0.556254770350074 *newDataPC[, 39 ] + 0.0532223671592434 *newDataPC[, 40 ] + -0.276465936790609 *newDataPC[, 41 ] + -0.509199877225203 *newDataPC[, 42 ] + -0.148773640506358 *newDataPC[, 43 ] + -1.10192628874646 *newDataPC[, 44 ] + 0.324912371967778 *newDataPC[, 45 ] + -0.313158025818722 *newDataPC[, 46 ] + -0.115894882917378 *newDataPC[, 47 ] + 0.664838278247416 *newDataPC[, 48 ] + 0.0501012801559784 *newDataPC[, 49 ] + -0.271350987334992 *newDataPC[, 50 ] + 0.0712147370410782 *newDataPC[, 51 ] + -0.186887783921391 *newDataPC[, 52 ] + -1.03147137540596 *newDataPC[, 53 ] + 1.84310524480822 *newDataPC[, 54 ] + -0.186548494025842 *newDataPC[, 55 ] + 0.366154611497872 *newDataPC[, 56 ] + 0.896442948963268 *newDataPC[, 57 ] + -0.338837755929702 *newDataPC[, 58 ] + -0.108965143693738 *newDataPC[, 59 ] + -0.622177560755763 *newDataPC[, 60 ] + -0.492837647639474 *newDataPC[, 61 ] + 0.36499968959849 *newDataPC[, 62 ] + 0.149351333420691 *newDataPC[, 63 ] + 0.0267663631445083 *newDataPC[, 64 ]

#take logits and exponentiate them
logits=cbind(logitBlues,logitClassical,logitJazz,logitMetal,logitPop,logitRock)
#calculate probabilites for all, knowing that they must sum to 1
p.unscaled=exp(logits)
p=(p.unscaled / rowSums(p.unscaled))
#p is a table with the probabilities for each type of music
#we want to predict the genre with the highest probability 
pred=rep(0,N)
for (i in 1:N){
predictedGenre=which(p[i,]==max(p[i,]));
if (predictedGenre ==1){
pG='Blues'
}
if (predictedGenre ==2){
pG='Classical'
}
if (predictedGenre ==3){
pG='Jazz'
}
if (predictedGenre ==4){
pG='Metal'
}
if (predictedGenre ==5){
pG='Pop'
}
if (predictedGenre ==6){
pG='Rock'
}
pred[i]=pG
}

#output predicted probabilities table with predictions
p=cbind(p,pred)

##########################################################################################
#MODEL 2- 19PCS
##########################################################################################
#Set up prediction equations for model2
N=dim(newDataPC)[1]

y=coef(model2)

logitBlues=rep(0,N);
logitClassical=y[1] + -0.89654010903019 *newDataPC[, 1 ] + -0.359637900274023 *newDataPC[, 2 ] + 0.199046058274548 *newDataPC[, 3 ] + 0.563255028966522 *newDataPC[, 4 ] + 0.566373032557467 *newDataPC[, 5 ] + -0.306291919060971 *newDataPC[, 6 ] + 0.0559204648996901 *newDataPC[, 7 ] + 0.984643756039183 *newDataPC[, 8 ] + -0.0718592020758667 *newDataPC[, 9 ] + -0.278338937338042 *newDataPC[, 10 ] + 0.103094102335677 *newDataPC[, 11 ] + 0.530169742791329 *newDataPC[, 12 ] + 0.146689478318829 *newDataPC[, 13 ] + 0.0492165464486468 *newDataPC[, 14 ] + 0.441052402008421 *newDataPC[, 15 ] + -0.151161776563035 *newDataPC[, 16 ] + 0.0321890008070155 *newDataPC[, 17 ] + 0.48033743869777 *newDataPC[, 18 ] + 0.251140600687623 *newDataPC[, 19 ]
logitJazz=y[2]+ -0.553715279605659 *newDataPC[, 1 ] + -0.129181493638924 *newDataPC[, 2 ] + 0.36551532182019 *newDataPC[, 3 ] + 0.505312469445006 *newDataPC[, 4 ] + 0.227619642723444 *newDataPC[, 5 ] + -0.418906371602234 *newDataPC[, 6 ] + -0.0372472442634389 *newDataPC[, 7 ] + 0.875258082608679 *newDataPC[, 8 ] + -0.301270330432817 *newDataPC[, 9 ] + -0.0754211501737677 *newDataPC[, 10 ] + 0.0452960256659095 *newDataPC[, 11 ] + 0.480423169299138 *newDataPC[, 12 ] + 0.0748696697573835 *newDataPC[, 13 ] + -0.0850005959191618 *newDataPC[, 14 ] + 0.323323245418224 *newDataPC[, 15 ] + -0.243067401430909 *newDataPC[, 16 ] + 0.0740912637412682 *newDataPC[, 17 ] + 0.289027347626288 *newDataPC[, 18 ] + 0.392262726832334 *newDataPC[, 19 ]
logitMetal=y[3]+ 0.496643476279007 *newDataPC[, 1 ] + -0.230316422060739 *newDataPC[, 2 ] + -0.0864892270773636 *newDataPC[, 3 ] + -0.0208263689664312 *newDataPC[, 4 ] + -0.344097516943239 *newDataPC[, 5 ] + 0.232145450281671 *newDataPC[, 6 ] + 0.338946318018523 *newDataPC[, 7 ] + 0.685719068483736 *newDataPC[, 8 ] + -0.508420178674387 *newDataPC[, 9 ] + -0.189488244295874 *newDataPC[, 10 ] + -0.260719551071813 *newDataPC[, 11 ] + 0.314241203640011 *newDataPC[, 12 ] + -0.385788214083712 *newDataPC[, 13 ] + -0.194962697608977 *newDataPC[, 14 ] + -0.650146596513761 *newDataPC[, 15 ] + -0.299096034305591 *newDataPC[, 16 ] + 0.0379931679085811 *newDataPC[, 17 ] + -0.10865684349571 *newDataPC[, 18 ] + -0.00727858713541186 *newDataPC[, 19 ]
logitPop=y[4]+ 0.144316346305768 *newDataPC[, 1 ] + 0.311586639573168 *newDataPC[, 2 ] + 0.286417236188983 *newDataPC[, 3 ] + 0.423989967798061 *newDataPC[, 4 ] + -0.850901116946443 *newDataPC[, 5 ] + -0.0987172873528272 *newDataPC[, 6 ] + -0.124339013012277 *newDataPC[, 7 ] + 0.928398165083073 *newDataPC[, 8 ] + -0.729766715909813 *newDataPC[, 9 ] + 0.156152537596126 *newDataPC[, 10 ] + -0.187883075202381 *newDataPC[, 11 ] + 0.662235705229916 *newDataPC[, 12 ] + -0.197150341246302 *newDataPC[, 13 ] + -0.251049474169806 *newDataPC[, 14 ] + -0.0646966373234829 *newDataPC[, 15 ] + 0.0623860660774771 *newDataPC[, 16 ] + 0.0964996296033383 *newDataPC[, 17 ] + 0.152841384881701 *newDataPC[, 18 ] + -0.0228793256562379 *newDataPC[, 19 ]
logitRock=y[5]+ 0.28368780360012 *newDataPC[, 1 ] + -0.125885255875159 *newDataPC[, 2 ] + -0.0478414836005371 *newDataPC[, 3 ] + 0.330956347350021 *newDataPC[, 4 ] + -0.568822298789751 *newDataPC[, 5 ] + -0.14835290136216 *newDataPC[, 6 ] + -0.295837767822687 *newDataPC[, 7 ] + 0.350841488827907 *newDataPC[, 8 ] + -0.34242608685238 *newDataPC[, 9 ] + 0.13707235509285 *newDataPC[, 10 ] + -0.430567288152942 *newDataPC[, 11 ] + 0.701011309939405 *newDataPC[, 12 ] + 0.0233105378069691 *newDataPC[, 13 ] + -0.0357910609223557 *newDataPC[, 14 ] + 0.38267547181488 *newDataPC[, 15 ] + -0.223691504416597 *newDataPC[, 16 ] + 0.140270047036746 *newDataPC[, 17 ] + 0.606815291645962 *newDataPC[, 18 ] + 0.176029646703654 *newDataPC[, 19 ] 

#take logits and exponentiate them
logits=cbind(logitBlues,logitClassical,logitJazz,logitMetal,logitPop,logitRock)
#calculate probabilites for all, knowing that they must sum to 1
p.unscaled=exp(logits)
p=(p.unscaled / rowSums(p.unscaled))
#p is a table with the probabilities for each type of music
#we want to predict the genre with the highest probability 
pred=rep(0,N)
for (i in 1:N){
predictedGenre=which(p[i,]==max(p[i,]));
if (predictedGenre ==1){
pG='Blues'
}
if (predictedGenre ==2){
pG='Classical'
}
if (predictedGenre ==3){
pG='Jazz'
}
if (predictedGenre ==4){
pG='Metal'
}
if (predictedGenre ==5){
pG='Pop'
}
if (predictedGenre ==6){
pG='Rock'
}
pred[i]=pG
}

#output predicted probabilities table with predictions
p=cbind(p,pred)

##########################################################################################
#MODEL 3- 30PCS
##########################################################################################
#Set up prediction equations for model2
N=dim(newDataPC)[1]

y=coef(model3)

logitBlues=rep(0,N);
logitClassical=y[1]+ -1.18433730176279 *newDataPC[, 1 ] + -0.445987406316334 *newDataPC[, 2 ] + 0.389765619569254 *newDataPC[, 3 ] + 0.690998359347961 *newDataPC[, 4 ] + 0.666740951219003 *newDataPC[, 5 ] + -0.445767678308755 *newDataPC[, 6 ] + -0.0975763207570145 *newDataPC[, 7 ] + 1.17752487441909 *newDataPC[, 8 ] + 0.308165677844672 *newDataPC[, 9 ] + -0.397445317576371 *newDataPC[, 10 ] + 0.0229408810841151 *newDataPC[, 11 ] + 0.32755118324985 *newDataPC[, 12 ] + 0.392212580593077 *newDataPC[, 13 ] + 0.200968874409785 *newDataPC[, 14 ] + 0.845234011556136 *newDataPC[, 15 ] + -0.0379062632618432 *newDataPC[, 16 ] + -0.0591289050469473 *newDataPC[, 17 ] + 0.906042990780667 *newDataPC[, 18 ] + 0.194775180826801 *newDataPC[, 19 ] + -0.0865230881705315 *newDataPC[, 20 ] + 1.15838618383143 *newDataPC[, 21 ] + 0.644103793713482 *newDataPC[, 22 ] + -0.597139089081226 *newDataPC[, 23 ] + -0.0676962943901615 *newDataPC[, 24 ] + -1.27836255752932 *newDataPC[, 25 ] + 0.693949374013212 *newDataPC[, 26 ] + 0.449883368742874 *newDataPC[, 27 ] + -0.259749927858875 *newDataPC[, 28 ] + 0.00430175490195974 *newDataPC[, 29 ] + 0.0987218849541408 *newDataPC[, 30 ]
logitJazz=y[2]+ -0.686224475082478 *newDataPC[, 1 ] + -0.139182131419311 *newDataPC[, 2 ] + 0.518189257273161 *newDataPC[, 3 ] + 0.624366578345962 *newDataPC[, 4 ] + 0.239034746102148 *newDataPC[, 5 ] + -0.491670572988787 *newDataPC[, 6 ] + -0.195828033984431 *newDataPC[, 7 ] + 1.19382392149285 *newDataPC[, 8 ] + -0.0245676594833682 *newDataPC[, 9 ] + -0.150120156939159 *newDataPC[, 10 ] + 0.0910698634803345 *newDataPC[, 11 ] + 0.171357088680912 *newDataPC[, 12 ] + 0.167677956934739 *newDataPC[, 13 ] + -0.100266178999164 *newDataPC[, 14 ] + 0.593454406537295 *newDataPC[, 15 ] + -0.0639182995834543 *newDataPC[, 16 ] + 0.0292618606408321 *newDataPC[, 17 ] + 0.633492347565187 *newDataPC[, 18 ] + 0.41017445570424 *newDataPC[, 19 ] + 0.0964903551728393 *newDataPC[, 20 ] + 0.978102764995406 *newDataPC[, 21 ] + 0.80992246053389 *newDataPC[, 22 ] + -0.36025569453517 *newDataPC[, 23 ] + -0.0705097423896014 *newDataPC[, 24 ] + -1.30486195902399 *newDataPC[, 25 ] + -0.0167895466948305 *newDataPC[, 26 ] + 0.24047248551354 *newDataPC[, 27 ] + -0.437227923798595 *newDataPC[, 28 ] + -0.352539321706659 *newDataPC[, 29 ] + 0.228726716270978 *newDataPC[, 30 ]
logitMetal=y[3]+ 0.635252378593417 *newDataPC[, 1 ] + -0.269774545163556 *newDataPC[, 2 ] + 0.0091237946598052 *newDataPC[, 3 ] + 0.020170844569185 *newDataPC[, 4 ] + -0.25853602980946 *newDataPC[, 5 ] + 0.241765675674583 *newDataPC[, 6 ] + 0.21662812746613 *newDataPC[, 7 ] + 0.839804192353677 *newDataPC[, 8 ] + -0.632290371724519 *newDataPC[, 9 ] + -0.348731565836758 *newDataPC[, 10 ] + -0.097878447698261 *newDataPC[, 11 ] + 0.136781270491507 *newDataPC[, 12 ] + -0.487928819883728 *newDataPC[, 13 ] + -0.320802666083495 *newDataPC[, 14 ] + -0.71967711874706 *newDataPC[, 15 ] + -0.357076293343935 *newDataPC[, 16 ] + 0.204762517164954 *newDataPC[, 17 ] + -0.0892633078579086 *newDataPC[, 18 ] + -0.0242030478836302 *newDataPC[, 19 ] + 0.606545281896908 *newDataPC[, 20 ] + 0.339175691456229 *newDataPC[, 21 ] + 0.437530199813755 *newDataPC[, 22 ] + -0.0679052805794901 *newDataPC[, 23 ] + -0.107991122386677 *newDataPC[, 24 ] + -0.13443816044737 *newDataPC[, 25 ] + -0.405389098542161 *newDataPC[, 26 ] + 0.0767063296990708 *newDataPC[, 27 ] + -0.902903576026848 *newDataPC[, 28 ] + -0.952036070380258 *newDataPC[, 29 ] + -0.313255889110956 *newDataPC[, 30 ]
logitPop=y[4]+ 0.186995798602742 *newDataPC[, 1 ] + 0.336842731046719 *newDataPC[, 2 ] + 0.339807812016592 *newDataPC[, 3 ] + 0.51978051938278 *newDataPC[, 4 ] + -0.910124253635056 *newDataPC[, 5 ] + -0.101318549332906 *newDataPC[, 6 ] + -0.141752671557785 *newDataPC[, 7 ] + 1.04295371038988 *newDataPC[, 8 ] + -0.622437416995753 *newDataPC[, 9 ] + 0.0588867591687066 *newDataPC[, 10 ] + -0.138771444519021 *newDataPC[, 11 ] + 0.570226641421159 *newDataPC[, 12 ] + -0.279837658134749 *newDataPC[, 13 ] + -0.295099008286775 *newDataPC[, 14 ] + 0.0326089273051087 *newDataPC[, 15 ] + 0.146123303389017 *newDataPC[, 16 ] + 0.107960211823093 *newDataPC[, 17 ] + 0.260695698288235 *newDataPC[, 18 ] + -0.0693519158943841 *newDataPC[, 19 ] + 0.3828896227577 *newDataPC[, 20 ] + 0.0324024085254958 *newDataPC[, 21 ] + 0.821627820680966 *newDataPC[, 22 ] + -0.0352644966888325 *newDataPC[, 23 ] + -0.181541443874957 *newDataPC[, 24 ] + -0.571006350869158 *newDataPC[, 25 ] + -0.227433037480323 *newDataPC[, 26 ] + 0.0418495744275331 *newDataPC[, 27 ] + -0.439733198802347 *newDataPC[, 28 ] + -0.454231630318178 *newDataPC[, 29 ] + -0.0725510687644089 *newDataPC[, 30 ]
logitRock=y[5]+ 0.313687525018657 *newDataPC[, 1 ] + -0.138682725684782 *newDataPC[, 2 ] + 0.023040240580418 *newDataPC[, 3 ] + 0.394608368479381 *newDataPC[, 4 ] + -0.614835294880728 *newDataPC[, 5 ] + -0.15932954479916 *newDataPC[, 6 ] + -0.304575977354292 *newDataPC[, 7 ] + 0.456844377664972 *newDataPC[, 8 ] + -0.296373298588988 *newDataPC[, 9 ] + 0.0453956808605629 *newDataPC[, 10 ] + -0.358327553777103 *newDataPC[, 11 ] + 0.644737508310657 *newDataPC[, 12 ] + 0.0701904094550391 *newDataPC[, 13 ] + -0.111678974787088 *newDataPC[, 14 ] + 0.387969168729391 *newDataPC[, 15 ] + -0.0258973102010801 *newDataPC[, 16 ] + 0.083424396057122 *newDataPC[, 17 ] + 0.738862549224976 *newDataPC[, 18 ] + 0.10438063736506 *newDataPC[, 19 ] + 0.333313610362567 *newDataPC[, 20 ] + 0.274762864803086 *newDataPC[, 21 ] + 0.679447213589995 *newDataPC[, 22 ] + -0.458489623579331 *newDataPC[, 23 ] + -0.338807170992717 *newDataPC[, 24 ] + -0.853225571550273 *newDataPC[, 25 ] + -0.106269312720588 *newDataPC[, 26 ] + -0.425146000398407 *newDataPC[, 27 ] + -0.122540984602328 *newDataPC[, 28 ] + -0.455968802704175 *newDataPC[, 29 ] + 0.253948613023965 *newDataPC[, 30 ]

#take logits and exponentiate them
logits=cbind(logitBlues,logitClassical,logitJazz,logitMetal,logitPop,logitRock)
#calculate probabilites for all, knowing that they must sum to 1
p.unscaled=exp(logits)
p=(p.unscaled / rowSums(p.unscaled))
#p is a table with the probabilities for each type of music
#we want to predict the genre with the highest probability 
pred=rep(0,N)
for (i in 1:N){
predictedGenre=which(p[i,]==max(p[i,]));
if (predictedGenre ==1){
pG='Blues'
}
if (predictedGenre ==2){
pG='Classical'
}
if (predictedGenre ==3){
pG='Jazz'
}
if (predictedGenre ==4){
pG='Metal'
}
if (predictedGenre ==5){
pG='Pop'
}
if (predictedGenre ==6){
pG='Rock'
}
pred[i]=pG
}

#output predicted probabilities table with predictions
p=cbind(p,pred)

