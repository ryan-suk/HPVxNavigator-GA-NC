library(shiny)
library(shinythemes)
library(dplyr)
library(geosphere)  # for distHaversine
library(tibble)

# Load provider data for Georgia
providers_ga <- read.csv("www/providers_ga_geocoded.csv", stringsAsFactors = FALSE)

# Rename columns to match what the app expects
providers_ga <- providers_ga %>%
  rename(
    name = provider_name,
    address1 = street_address_1,
    address2 = street_address_2,  # or remove if unnecessary
    city = city,
    state = state,
    zip = zipcode,
    phone = phone,
    email = email
  )

# UI
ui <- navbarPage("HPVx Navigator: GA & NC",
                 id = "main_tabs", 
                 theme = shinytheme("flatly"),
                 
                 # --- Tab 1: Eligibility Screener ---
                 tabPanel("Eligibility Screener", value = "screening_tab",
                          sidebarLayout(
                            sidebarPanel(
                              # Main logo
                              div(style = "text-align:center; margin-bottom:20px;",
                                  img(src = "HPVxNavigator.png", height = "120px")
                              ),
                              
                              selectInput("state", "Select Your State", 
                                          choices = c("Choose..." = "", "Georgia", "North Carolina")),
                              
                              numericInput("age", "Enter Your -Or Your Child's- Age", value = NA, min = 9, max = 100),
                              
                              selectInput("insurance", "Do you have insurance?", 
                                          choices = c("Choose..." = "", "Yes", "No")),
                              
                              conditionalPanel(
                                condition = "input.insurance == 'Yes'",
                                selectInput("insurance_type", "What type of insurance do you have?", 
                                            choices = c("Private", "Medicaid", "PeachCare/NC Health Choice", "Other"))
                              ),
                              
                              conditionalPanel(
                                condition = "input.insurance == 'Yes'",
                                selectInput("vaccine_coverage", "Does your insurance cover vaccines?", 
                                            choices = c("Yes", "No", "Not sure"))
                              ),
                              
                              actionButton("check_eligibility", "Check Eligibility")
                            ),
                            
                            mainPanel(
                              # Sub logos top right
                              fluidRow(
                                column(9),
                                column(3,
                                       div(style = "text-align:right; margin-bottom:10px;",
                                           img(src = "HPVxNaviGA.png", height = "60px", style = "margin-right:10px;"),
                                           img(src = "HPVxNaviNC.png", height = "60px")
                                       )
                                )
                              ),
                              
                              htmlOutput("eligibility_result")
                            )
                          )
                 ),
                 
                 # --- Tab 2: Vaccination Pathway Finder ---
                 tabPanel("Vaccination Pathway Finder", value = "pathway_tab",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("path_state", "State", choices = c("Choose..." = "", "Georgia", "North Carolina")),
                              
                              # <-- Ensure this is directly in the sidebarPanel
                              uiOutput("ga_zip_input"),
                              
                              selectInput("transportation", "Do you need help with transportation?", choices = c("Choose..." = "", "Yes", "No")),
                              actionButton("show_steps", "Show Steps")
                            ),
                            mainPanel(
                              uiOutput("pathway_steps")
                            )
                          )
                 ),
                 
                 # --- Tab 3: Community Resources Match ---
                 tabPanel("Community Resources", value = "resource_tab",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("resource_state", "Select State", choices = c("Georgia", "North Carolina")),
                              checkboxGroupInput("needs", "Select your needs", 
                                                 choices = c("Transportation", "Financial Help", "Language Support"))
                            ),
                            mainPanel(
                              uiOutput("resource_links")
                            )
                          )
                 )
)

# Server
server <- function(input, output, session) {
  
  clicked <- reactiveVal(FALSE)
  observeEvent(input$check_eligibility, {
    clicked(TRUE)
  })
  
  # --- Eligibility Screener Logic ---
  output$eligibility_result <- renderUI({
    if (!clicked()) {
      return(HTML(" <div style='font-size:16px; font-weight:bold;'>
        Welcome to HPVx Navigator State Focus Hub: GA & NC!<br>
        Please complete the screener to check your HPV vaccine eligibility.<br><br>
        Contacts:<br>
        Ryan Suk, PhD 
        <a href='mailto:ryan.suk@emory.edu' style='color:blue; text-decoration:underline;'>
          ryan.suk@emory.edu
        </a> — For questions related to this dashboard and Georgia content<br>
        Rachael Baartmans, MPH 
        <a href='mailto:rachael.baartmans@dhhs.nc.gov' style='color:blue; text-decoration:underline;'>
          rachael.baartmans@dhhs.nc.gov
        </a> — For questions related to North Carolina content
      </div>
      <br><img src='infographic1.png' height='370px'>
      <br><br><div style='font-size:16px; font-weight:bold;'>SHINY APP POWERED BY..</div>
      <img src='lab_logo.png' height='90px'><br>"))
    }
    
    message <- ""
    image <- ""
    
    if (input$state == "" || is.na(input$age) || input$insurance == "") {
      message <- "ℹ️ Please complete all required fields above before checking eligibility."
      image <- "placeholder.png"
    } else if (input$age < 19) {
      if (input$age < 9) {
        return(HTML(paste0(
          "<div style='font-size:16px;'>",
          "ℹ HPV vaccination is recommended starting at age 9. ",
          "Please check back when your child turns 9 and speak to a healthcare provider in ", input$state, " for guidance on HPV vaccination.",
          "</div><br>",
          "<img src='atnine.png' height='350px'><br><br>",
          "<img src='timeline.png' height='230px'>"
        )))
        
      } else {  # age 9–18
        if (input$insurance == "No") {
          message <- paste("✅ You are eligible for free HPV vaccination through the VFC program in", input$state,
                           '. Check the <a href="#" onclick="$(\'a[data-value=\\\'pathway_tab\\\']\').tab(\'show\')" style="color:blue; text-decoration:underline;">Vaccination Pathway Finder</a>.')
          image <- "placeholder.png"
          
        } else if (input$insurance == "Yes") {
          if (!is.null(input$insurance_type) && input$insurance_type == "Medicaid") {
            message <- paste("✅ Since you have Medicaid and are under 19, you qualify for the VFC program in", input$state,
                             '. Check the <a href="#" onclick="$(\'a[data-value=\\\'pathway_tab\\\']\').tab(\'show\')" style="color:blue; text-decoration:underline;">Vaccination Pathway Finder</a>.')
            image <- "placeholder.png"
            
          } else if (!is.null(input$vaccine_coverage)) {
            if (input$vaccine_coverage == "No") {
              message <- paste("✅ Your insurance does not cover vaccines, so you may qualify for the VFC program in", input$state,
                               '. Check the <a href="#" onclick="$(\'a[data-value=\\\'pathway_tab\\\']\').tab(\'show\')" style="color:blue; text-decoration:underline;">Vaccination Pathway Finder</a>.')
              image <- "placeholder.png"
              
            } else if (input$vaccine_coverage == "Yes") {
              message <- paste("ℹ️ Since your insurance covers vaccines, you can get the HPV vaccine through your regular provider in", input$state)
              image <- "placeholder.png"
              
            } else {
              message <- paste("❓ If you're unsure about your vaccine coverage, contact your insurance provider or ask a clinic in", input$state,
                               ". You may still be eligible for VFC.")
              image <- "placeholder.png"
            }
            
          } else {
            message <- "ℹ️ Please answer all insurance-related questions to check eligibility."
            image <- "placeholder.png"
          }
        }
      }
    } else if (input$age >= 19) {
      if (input$age <= 26) {
        if (input$insurance == "No") {
          message <- paste("✅ You may be eligible for free or low-cost HPV vaccination through safety-net providers in", input$state,
                           ". HPV vaccination is routinely recommended through age 26. Visit the Vaccination Pathway Finder.")
        } else if (input$insurance == "Yes") {
          if (input$vaccine_coverage == "Yes") {
            message <- paste("✅ HPV vaccination is recommended through age 26. Your insurance likely covers it — schedule with your provider in", input$state)
          } else if (input$vaccine_coverage == "No") {
            message <- paste("ℹ️ HPV vaccination is recommended through age 26. Since your insurance doesn’t cover it, explore sliding-scale providers in", input$state,
                             "via the Vaccination Pathway Finder.")
          } else {
            message <- paste("❓ HPV vaccination is recommended through age 26. Unsure about coverage? Ask your insurer or a provider in", input$state)
          }
        }
        
      } else if (input$age <= 45) {  # Ages 27–45
        if (input$insurance == "No") {
          message <- paste0(
            "ℹ️ HPV vaccination for ages 27–45 is based on shared clinical decision-making. If uninsured, explore low-cost providers in ", input$state,
            " through <a href='https://www.merckhelps.com/gardasil%209' target='_blank' style='color:blue; text-decoration:underline;'>The Merck Patient Assistance Program</a>."
          )
        } else if (input$insurance == "Yes") {
          if (input$vaccine_coverage == "Yes") {
            message <- paste("✅ You are eligible for HPV vaccination based on shared decision-making. Your insurance likely covers it — ask your provider in", input$state)
          } else if (input$vaccine_coverage == "No") {
            message <- paste("ℹ️ You are eligible for HPV vaccination based on shared decision-making. Since your insurance doesn’t cover it, check with providers in", input$state,
                             "for pricing and eligibility.")
          } else {
            message <- paste("❓You are eligible for HPV vaccination based on shared decision-making. Check with your insurer or provider in", input$state,
                             "to discuss coverage and need.")
          }
        }
        
      } else {  # Over 45
        if (input$insurance == "No") {
          message <- paste("ℹ️ HPV vaccination is not recommended over age 45. If you're still interested, consult a provider in", input$state,
                           "for eligibility and cost options.")
        } else if (input$insurance == "Yes") {
          if (input$vaccine_coverage == "Yes") {
            message <- paste("✅ Though not recommended, your insurance may cover HPV vaccination. Talk to your provider in", input$state)
          } else if (input$vaccine_coverage == "No") {
            message <- paste("ℹ️ HPV vaccination is not recommended over 45, and your insurance may not cover it. Ask a provider in", input$state,
                             "about alternatives.")
          } else {
            message <- paste("❓HPV vaccination is not recommended over 45. Consult a provider in", input$state,
                             "for guidance.")
          }
        }
      }
      
      image <- "timeline.png"
    }
    
    
    HTML(paste0("<div style='font-size:16px;'>", message, "</div><br><img src='", image, "' height='350px'>"))   
  })
  
  # --- Vaccination Pathway Steps ---
  # Show ZIP code input for Georgia
  # Show ZIP input only for Georgia
  output$ga_zip_input <- renderUI({
    if (input$path_state == "Georgia") {
      textInput("zip_input", "Enter Your ZIP Code (Georgia only):", placeholder = "e.g., 30303")
    }
  })
  
  # Geocoding table (example: preload ZIPs with lat/lon)
  zip_coords <- read.csv("www/zip_ga.csv", stringsAsFactors = FALSE)  # Must contain: zip, lat, lon
  
  observeEvent(input$show_steps, {
    output$pathway_steps <- renderUI({
      if (input$path_state == "Georgia") {
        user_zip <- input$zip_input
        
        if (!is.null(user_zip) && user_zip != "") {
          user_location <- zip_coords %>% filter(zip == user_zip)
          
          # Try ±1 ZIP if not found
          if (nrow(user_location) == 0) {
            zip_range <- as.character(as.numeric(user_zip) + c(-1, 0, 1))
            user_location <- zip_coords %>% filter(zip %in% zip_range)
          }
          
          if (nrow(user_location) >= 1) {
            user_latlon <- c(user_location$lon[1], user_location$lat[1])  # Use the first valid one
            
            # Calculate distances
            providers_ga$distance_miles <- distHaversine(
              matrix(c(providers_ga$lon, providers_ga$lat), ncol = 2),
              user_latlon
            ) * 0.000621371  # meters to miles
            
            nearest <- providers_ga %>%
              arrange(distance_miles) %>%
              head(10)
            
            intro_msg <- if (input$transportation == "Yes") {
              paste0(
                "<b>Below are nearby providers you might consider:</b><br>",
                "🚗 You can also explore local transit options in the ",
                "<a href='#' onclick=\"$('a[data-value=\\'resource_tab\\']').tab('show')\" style='color:blue; text-decoration:underline;'>Community Resources</a> tab.<br><br>"
              )
            } else {
              "<b>Here are providers near you.</b><br><br>"
            }
            
            html_output <- paste0("<h4>Nearest Providers to ZIP ", user_zip, ":</h4>",
                                  intro_msg, 
                                  "<ul>",
                                  paste0(
                                    "<li><b>", nearest$name, "</b><br>",
                                    nearest$address1, ifelse(nearest$address2 != "", paste0(", ", nearest$address2), ""), "<br>",
                                    nearest$city, ", ", nearest$state, " ", nearest$zip, "<br>",
                                    "📞 ", nearest$phone, "<br>",
                                    sprintf("📍 %.1f miles away", nearest$distance_miles),
                                    "</li><br>", collapse = ""),
                                  "</ul>"
                                 )
            
            HTML(html_output)
          } else {
            HTML("<b>ZIP code not found. Please enter a nearby Georgia ZIP (or valid numeric ZIP code).</b>")
          }
        } else {
          HTML("<b>Please enter a ZIP code to see providers in Georgia.</b>")
        }
        
      } else if (input$path_state == "North Carolina") {
        intro_msg_nc <- if (input$transportation == "Yes") {
          paste0(
            "<b>Here are steps to find a provider in North Carolina:</b><br>",
            "🚗 Since you need help with transportation, explore options in the ",
            "<a href='#' onclick=\"$('a[data-value=\\'resource_tab\\']').tab('show')\" style='color:blue; text-decoration:underline;'>Community Resources</a> tab.<br><br>"
          )
        } else {
          "<b>Here are steps to find a provider in North Carolina:</b><br><br>"
        }
        
        HTML(paste0(
          intro_msg_nc,
          "<ol>
      <li>Visit the NC VFC Program: <a href='https://www.dph.ncdhhs.gov/programs/epidemiology/immunization/you-and-your-family/vaccines-for-children-program' target='_blank'>NC VFC Program</a></li>
      <li>Search for providers by county or ZIP code</li>
      <li>Call a provider to confirm they offer the HPV vaccine</li>
      <li>Ask about low-cost or sliding scale options and schedule your visit</li>
    </ol>"
        ))
      }
    })
  })

  
  # --- Community Resources Match ---
  output$resource_links <- renderUI({
    links <- list()
    
    if (input$resource_state == "Georgia") {
      if ("Transportation" %in% input$needs) {
        links <- c(links, "<a href='https://www.navigateresources.net/uwcg/search.aspx' target='_blank'>🚗 2-1-1 GA Resources Search Including Transporation</a><br>")
      }
      if ("Financial Help" %in% input$needs) {
        links <- c(links, "<a href='https://www.navigateresources.net/uwcg/search.aspx' target='_blank'>💰 2-1-1 GA Resources Search Including Financial Assistance</a><br>")
      }
      if ("Language Support" %in% input$needs) {
        links <- c(
          links,
          "<a href='https://dhs.georgia.gov/organization/about/language-access' target='_blank'>🗣️ Georgia Language Support Services</a><br>",
          "<a href='https://www.navigateresources.net/uwcg/search.aspx' target='_blank'>🌐 2-1-1 GA Resources Search Including Language Services</a><br>"
        )
      }
    } else if (input$resource_state == "North Carolina") {
      if ("Transportation" %in% input$needs) {
        links <- c(links, "<a href='https://nc211.org/search/?keyword=transportation&location=NC++north+carolina&distance=100&skip=0&subtopic=Transportation&topic=Basic+Needs&taxonomyCode=' target='_blank'>🚗 NC Public Transportation</a><br>")
      }
      if ("Financial Help" %in% input$needs) {
        links <- c(links, "<a href='https://nc211.org/search/?keyword=Income%20Support%20and%20Employment&location=NC%20%20north%20carolina&distance=100&skip=0&subtopic=Employment%2CPublic%20Assistance%20Programs%2CSocial%20Insurance%20Programs%2CTemporary%20Financial%20Assistance&topic=Income%20Support%20and%20Employment&taxonomyCode=' target='_blank'>💰 NC DHHS Financial Support</a><br>")
      }
      if ("Language Support" %in% input$needs) {
        links <- c(links, "<a href='https://www.ncdhhs.gov/assistance/notice-informing-individuals-about-nondiscrimination-and-accessibility-requirements' target='_blank'>🗣️ NC Language Services</a><br>")
      }
    }
    
    HTML(paste(links, collapse = ""))
  })


}

# Run the app
shinyApp(ui = ui, server = server)
