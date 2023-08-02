
# library required packages
library(shiny)
library(shinythemes)
library(bslib)
library(rclipboard)

# Unaltered scrambler function that performs the scramble
Scrambler <- function(Message, Seed = 123){
  
  # Splits the message into individual characters
  Mess_split = strsplit(Message, split = "")[[1]]
  
  # Scrambles the characters based on the given seed
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Mess_split),length(Mess_split),replace = FALSE)
  
  # Puts the characters back into a single element
  Mess_Scrammed = paste(Mess_split[Scrambed_Ind], collapse="")
  
  # Returns the scrambled message
  return(Mess_Scrammed)
}

# Unaltered unscrambler that undoes the scrambling
Unscrambler <- function(Scrambled, Seed = 123){
  
  # Breaks the scrambled text into individual characters
  Message_Scrammed = strsplit(Scrambled, split = "")[[1]]
  
  # Generates the same indexes that were used to scramble the message
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Message_Scrammed),length(Message_Scrammed),replace = FALSE)
  
  # Using those indexes the message is unscrambled
  Mess_Unscrammed = rep(0,length(Message_Scrammed))
  counter = 0
  for(i in Scrambed_Ind){
    counter = counter + 1
    Mess_Unscrammed[i] <- Message_Scrammed[counter]
  }
  
  # The message is made a single element
  Output = paste(Mess_Unscrammed, collapse = "")
  
  # Returns the message
  return(Output)
}

# Turns characters into numbers
Pass_Index <- function(Input){
  
  # Breaks the input into individual characters
  Input <- strsplit(Input,split = "")[[1]]
  
  # Sets the possible characters to be turned into indexes/numbers
  Pos <- as.character(c(""," ", letters,LETTERS,"?",".","!",0:9,"'",
                        "/","+","-","<",">","@","$","%","#","^","&",
                        "*","(",")","_","=",","))
  
  # Takes each characters and returns the corresponding index in Pos above
  for(i in 1:length(Input)){
    Counter = 0
    for(j in 1:length(Pos)){
      if(Counter == 0){
        if(Input[i] == Pos[j]){
          Input[i] = j; Counter = 1
        }
      }
    }
  }
  
  # Reports values 1-9 as 01-09
  for(j in 1:length(Input)){
    for(k in 1:9){
      if(Input[j] == k){
        Input[j] = paste("0",k,sep = "")
      }
    }
  }
  
  # Returns a vector of indexes
  return(Input)
}

# Function adapted to not scramble the original text.
Password_Protect <- function(Message, Password = "Crypto", Pass_Conf = "Crypto"){
  
  # Checks that the passwords match
  if(Password == Pass_Conf){
  Mess_init = Message
  
  # This turns the text based message into numbers using the Pass_Index function
  Message = paste(Pass_Index(Message),collapse = "")
  
  # This modifies the password to be 100 characters sampled from the original password
  # This process is made repeatable by making the seed a function of the original password
  set.seed(sum(as.numeric(Pass_Index(Password))))
  Pass_seq = sample(Pass_Index(Password),100,replace = TRUE)
  
  # This initializes a progress bar
  pb <- txtProgressBar(min = 0, max = length(Pass_seq), style = 3)
  
  # Scrambles using each password character index as a seed
  Counter = 0  
  for(i in Pass_seq){
    # For the progress bar
    Counter = Counter + 1
    setTxtProgressBar(pb,Counter)
    
    # Actual scrambling function
    Message = Scrambler(Message, Seed = i)
  }

  close(pb) # Ends the progress bar
  
  # Outputs a single space if the original text is present
  if(Mess_init == "Enter a Message!"){Message = " "}
  return(Message)
  }
  
  # Does none of the above and returns a warning if passwords do not match
  if(Password != Pass_Conf){return("Passwords do not match!")}
}

# Function adapted to not unscramble the original text.
Password_Remove <- function(Scrambled, Password = "Crypto"){
  
  # This code locks out the user after 3 incorrect attempts
  if(exists("invalid") == FALSE){assign("invalid",0,envir = .GlobalEnv)}
  if(invalid >= 3){
    Sys.sleep(30)
    assign("invalid",0, envir = .GlobalEnv)
  }
  
  # Resets the invalid index detection
  invalid_detected = FALSE
  
  # The initial value to not scramble default value
  Scrambled_init = Scrambled
  
  # Only runs the main code if the initial value is not present
  if(Scrambled_init != "Enter Your Numbers!"){
  
  # Like above this samples from the given password to make a 100 character password
  set.seed(sum(as.numeric(Pass_Index(Password))))
  Pass_seq = rev(sample(Pass_Index(Password),100,replace=TRUE)) #Indexes are reversed
  
  # Start of progress bar
  pb <- txtProgressBar(min = 0, max = length(Pass_seq), style = 3)
  
  # Unscrambles using each password character index as a seed
  Counter = 0
  for(i in Pass_seq){
    # For the progress bar
    Counter = Counter + 1
    setTxtProgressBar(pb,Counter)
    
    # Actually unscrambles the numbers
    Scrambled = Unscrambler(Scrambled, Seed = i)
  }

  close(pb) # Ends the progress bar
  
  # The possibilities vector used to turn the indexes back to characters
  Pos <- as.character(c(""," ", letters,LETTERS,"?",".","!",0:9,"'",
                        "/","+","-","<",">","@","$","%","#","^","&",
                        "*","(",")","_","=",","))
  
  # This splits the unscrambled numbers into chunks of 2, as they were initially
  Split = substring(Scrambled, seq(1,nchar(Scrambled)-1,2), seq(2, nchar(Scrambled), 2))
  
  # If the wrong password is provided invalid indexes are likely to exist.
  # These invalid indexes are 00 and 86-99, this code prevents a program error.
  # Additionally, the presence of these invalid indexes demonstrates that the...
  # provided password is incorrect, so the program can detect incorrect passwords...
  # without actually knowing the correct password. If the message is short...
  # an incorrect password may not be detected because invalid indexes do not occur.
  for(i in 1:length(Split)){
    if(Split[i] == "00" | Split[i] > length(Pos)){
      Split[i] = round(runif(1,1,length(Pos)),0) 
      invalid_detected = TRUE # Notes that an invalid index was found
    }
  }
  
  # The Pos and Split vectors are used to turn the indexes back into characters
  Output = rep(0, length(Split))
  for(i in 1:length(Output)){
    Output[i] = Pos[as.numeric(Split[i])]
  }
  Output = paste(Output, collapse = "")
  
  } # End of what happens if the value entered is not the default value
  
  # Simply outputs a space if the current value is the default value
  if(Scrambled_init == "Enter Your Numbers!"){Output = " "}
  
  # If an invalid index is found this code outputs a warning that the user will...
  # be locked out after more incorrect attempts. The invalid variable indexes by...
  # 0.5 because the code is unavoidably run twice, once for the output and once...
  # for the copy button (I have made attempts to eliminate this problem).
  if(invalid_detected == TRUE){
    assign("invalid",invalid+0.5, envir = .GlobalEnv)
    Output = paste("Invalid Password Detected! After",floor(3-invalid),
                   "more incorrect attempt(s) a 30 Second Lockout will be Imposed!")
  }
  
  # Returns the output
  return(Output)
}


# The user interface for the application
ui <- fluidPage(theme = shinytheme("cosmo"),
    rclipboardSetup(),
    navbarPage(
    # Title for overall application
    "Advanced Message in Numbers Cryptography",
    # Page for the scramble functionality
    tabPanel("Scramble",
      sidebarLayout(
          # Left side bar scramble function
          sidebarPanel(
              textInput("Mess",
                      "Enter Message",
                      value = "Enter a Message!"),
              textInput("Pass",
                      "Enter your Password",
                      value = "Crypto"),
              textInput("Conf",
                        "Confirm your Password",
                        value = "Crypto"),
              submitButton("Submit")
          ),
          # Right main panel scramble function
          mainPanel(
            h1("The Generated Numbers"),
            verbatimTextOutput("Encoded"),
            uiOutput("clip1"),
            h4("Made by statswithr.com")
          )
      ) 
     
    ),
    
    # Page for the unscramble functionality
    tabPanel("Unscramble",
             sidebarLayout(
               # Left side bar panel for the unscramble function
               sidebarPanel(
                 textInput("Scram",
                           "Enter the Generated Numbers",
                           value = "Enter Your Numbers!"),
                 textInput("Pass2",
                           "Enter your Password",
                           value = "Crypto"),
                 submitButton("Submit")
               ),
               # Right main panel for the unscramble function
               mainPanel(
                 h1("Decrypted Output"),
                 verbatimTextOutput("Decoded"),
                 uiOutput("clip2"),
                 h4("Made by statswithr.com")
               )
             ) 
            )
  )
)

# The server portion of the application
server <- function(input, output) {

  # The text output rendered on screen for encoding
  output$Encoded <- renderText({
   Password_Protect(Message = input$Mess, Password = input$Pass, Pass_Conf = input$Conf)
  })
  
  # The text output rendered on screen for decoding
  output$Decoded <- renderText({
   Password_Remove(Scrambled = input$Scram, Password = input$Pass2)
  })
  
  # The text output given to the copy button for encoding
  output$clip1 <- renderUI({
    rclipButton("clipbtn",label = "Copy Numbers",
      clipText = Password_Protect(Message = input$Mess, Password = input$Pass, Pass_Conf = input$Conf)
      )
  })
  
  # The text output given to the copy button for decoding
  output$clip2 <- renderUI({
    rclipButton("clipbtn",label = "Copy Message",
      clipText = Password_Remove(Scrambled = input$Scram, Password = input$Pass2)
      )
  })

}

# Connects the ui and server into a single application
shinyApp(ui = ui, server = server)
