#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα
DelayedFlights <- read_csv("F:/Desktop/Ergasia marinas/ergasia1/DelayedFlights.csv")

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
emptyLines <- sum(is.na(DelayedFlights))
print(emptyLines)

#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων
delFl<-matrix(, nrow = 3, ncol = 0)
for (month in 1:12){
  for (day in 1:max(DelayedFlights$DayofMonth[DelayedFlights$Month==month])){
    k<-sum(DelayedFlights$Month==month & DelayedFlights$DayofMonth==day)
    delFl<-cbind(delFl, c(k,month,day))
  }
}
tmp<-which.max(delFl[1,])
print(paste0("The most delays of flights occured at ", delFl[2,tmp], "/", delFl[3,tmp]))
    
#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008
meanDep<-vector()
for(month in 6:8){
  k<-vector()
  for (day in 1:max(DelayedFlights$DayofMonth[DelayedFlights$Month==month])){
    k<-c(k,sum(DelayedFlights$DepDelay[DelayedFlights$Month==month & DelayedFlights$DayofMonth==day]))
  }
  meanDep<-c(meanDep,mean(k))
}
print(paste0("The daily average delay of June is ", round(meanDep[1]),", for July is ", round(meanDep[2]),", for August is ", round(meanDep[3])))

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
findB<-DelayedFlights$CancellationCode=='B'
compNames<-DelayedFlights$UniqueCarrier[findB]
print(paste0("The company with the most cancellations Code B is ",names(which.max(table(compNames)))))

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
k<-which.max(table(DelayedFlights$FlightNum))
print(k)

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
print(names(which.max(table(DelayedFlights$Dest))))

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)
destName<-DelayedFlights$Dest[which.max(DelayedFlights$DepDelay)]
print(paste0("The destination with the biggest delay is ", destName))

#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών
lateAircraft<-!is.na(DelayedFlights$LateAircraftDelay>0)
names(which.max(table(DelayedFlights$UniqueCarrier[lateAircraft])))

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα
k<-vector()
for (month in 1:12){
  tmp<-sum(DelayedFlights$Month==month & DelayedFlights$DayofMonth==13 & DelayedFlights$CancellationCode=='A')
  k<-c(k,tmp)
}

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
k<-0
t<-0
for (day in 10:23){
  k<-k+sum(DelayedFlights$DepDelay[DelayedFlights$Month==4 & DelayedFlights$DayofMonth==day])
  t<-t+sum(DelayedFlights$Month==4 & DelayedFlights$DayofMonth==day)
}
print(k/t)

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
hours<-DelayedFlights$DepTime>600 & DelayedFlights$DepTime<1400
k<-max(DelayedFlights$SecurityDelay[hours & !is.na(DelayedFlights$SecurityDelay)])
print(DelayedFlights$Month[hours & DelayedFlights$SecurityDelay==k & !is.na(DelayedFlights$SecurityDelay)])

#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
time<-DelayedFlights$Month==11 & DelayedFlights$DayofMonth<=10
k<-min(DelayedFlights$ArrDelay[time & !is.na(DelayedFlights$ArrDelay)])
print(DelayedFlights$FlightNum[time & DelayedFlights$ArrDelay==k & !is.na(DelayedFlights$ArrDelay)])

#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
time<-DelayedFlights$Month==8 & DelayedFlights$DayofMonth>10 &  DelayedFlights$DayofMonth<=20
over30<-DelayedFlights$CarrierDelay>30 & !is.na(DelayedFlights$CarrierDelay)
depNames<-DelayedFlights$Origin[time & over30]
k<-names(which.max(table(depNames)))
print(k)

#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε
divertedArrived<-DelayedFlights$Diverted==1 & !is.na(DelayedFlights$ArrTime)
k<-DelayedFlights$FlightNum[divertedArrived]

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης






