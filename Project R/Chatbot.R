pizza_chatbot <- function() {
  print("Welcome to Pizza Restaurant! Order via this chat once completed, please type 'End'")
  username <- readline("What's your name: ")
  print(paste("Hello!", username))
  
  choice <- readline("Take out or Eat in? : ")
  
  x = list("end","End")
  orders <- list()
  '%!in%' <- Negate('%in%')
  repeat{
    i <- readline("Please order: ")
    if(i %!in% x){
      orders <- append(orders,i)}
    else{break}
  }
  print("This is order summary")
  print(paste("Take out/Eat in:",choice))
  for (o in orders){
    print(o)
  }
  print("We'll prepare your orders in 30 minutes. Have a nice day!")
}
