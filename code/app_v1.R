# Load required libraries
#install.packages("geosphere")
#install.packages("tibble")

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

# ---------- NEW: simple footer helper used across tabs ----------
footerUI <- function() {
  year <- format(Sys.Date(), "%Y")
  tagList(
    div(style = "height:36px;"),  # spacer above footer
    tags$footer(
      style = "margin-top:8px; padding:14px 0; text-align:center; color:#6b7280; border-top:1px solid #e5e7eb;",
      HTML(sprintf("&copy; %s HPVx Navigator: GA & NC. All rights reserved.", year))
    )
  )
}
# ----------------------------------------------------------------


# UI
ui <- navbarPage(
  # 🟢 CHANGED: Make the title clickable and send a Shiny event when clicked
  title = tags$span(
    "HPVx Navigator: GA & NC",
    id = "brand_home",
    style = "cursor:pointer; user-select:none;",
    onclick = "window.scrollTo({top:0, behavior:'smooth'}); 
                              Shiny.setInputValue('home_clicked', Date.now(), {priority:'event'});"
  ),
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
                                          choices = c("Choose..." = "", "Yes", "No", "Not sure")),
                              
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
                              
                              htmlOutput("eligibility_result"),
                              # ---------- NEW: footer on this tab ----------
                              footerUI()
                            )
                          )
                 ),
                 
                 # --- Tab 2: VFC Provider Finder ---
                 tabPanel("VFC Provider Finder", value = "pathway_tab",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("path_state", "State", choices = c("Choose..." = "", "Georgia", "North Carolina")),
                              
                              # <-- Ensure this is directly in the sidebarPanel
                              uiOutput("ga_zip_input"),
                              
                              selectInput("transportation", "Do you need help with transportation?", choices = c("Choose..." = "", "Yes", "No")),
                              actionButton("show_steps", "Show Steps")
                            ),
                            mainPanel(
                              uiOutput("pathway_steps"),
                              # ---------- NEW: footer on this tab ----------
                              footerUI()
                            )
                          )
                 ),
                 
                 # --- Tab 3: Community Resources Match ---
                 tabPanel("Community Resources", value = "resource_tab",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("resource_state", "Select State", choices = c("Choose..." = "", "Georgia", "North Carolina")),
                              checkboxGroupInput("needs", "Select your needs", 
                                                 choices = c("Transportation", "Financial Help", "Language Support"))
                            ),
                            mainPanel(
                              uiOutput("resource_links"),
                              
                              # 🔴 NEW: Scroll-hint overlay (appears when page has overflow and user isn't at bottom)
                              tags$style(HTML("
                                #scrollHint {
                                  position: fixed;
                                  bottom: 20px;
                                  left: 50%;
                                  transform: translateX(-50%);
                                  display: none;
                                  align-items: center;
                                  gap: 8px;
                                  padding: 8px 12px;
                                  background: rgba(255,255,255,0.95);
                                  border: 1px solid #e5e7eb;
                                  border-radius: 999px;
                                  box-shadow: 0 6px 16px rgba(0,0,0,.08);
                                  z-index: 9999;
                                  font-size: 14px;
                                  color: #374151;
                                }
                                #scrollHint .chev { font-size: 16px; animation: hintBounce 1.2s infinite ease-in-out; }
                                @keyframes hintBounce { 0%,100% { transform: translateY(0); } 50% { transform: translateY(4px); } }
                              ")),  # 🔴 NEW
                              
                              tags$div(  # 🔴 NEW
                                id = "scrollHint",
                                HTML("<span>Scroll for more</span>"),
                                tags$span(class = "chev", HTML("&#9660;"))  # ▼
                              ),
                              
                              tags$script(HTML("  // 🔴 NEW
                                (function() {
                                  function updateScrollHint() {
                                    var hint = document.getElementById('scrollHint');
                                    if (!hint) return;
                                    // Scope to this tab pane only
                                    var pane = $(hint).closest('.tab-pane');
                                    var visible = pane.is(':visible');
                                    if (!visible) { hint.style.display = 'none'; return; }
                                    var doc = document.documentElement;
                                    var hasOverflow = (doc.scrollHeight - doc.clientHeight) > 8;
                                    var nearBottom = (window.scrollY + window.innerHeight) >= (doc.scrollHeight - 8);
                                    hint.style.display = (hasOverflow && !nearBottom) ? 'flex' : 'none';
                                  }
                                  window.updateScrollHint = updateScrollHint;
                                  window.addEventListener('scroll', updateScrollHint, { passive: true });
                                  window.addEventListener('resize', updateScrollHint);
                                  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', updateScrollHint);
                                  setTimeout(updateScrollHint, 100);
                                })();
                              ")),  # 🔴 NEW
                              
                              
                               # ---------- NEW: footer on this tab ----------
                              footerUI()
                            )
                          )
                 ),
                 
                 # --- Tab 4: About Us ---  # NEW
                 tabPanel("About Us", value = "about_tab",
                          fluidRow(
                            column(
                              width = 8, offset = 2,
                              
                              # Header logo
                              div(
                                style = "text-align:center; margin-top:24px; margin-bottom:12px;",
                                img(src = "HPVxNavigator.png", height = "110px"),
                                tags$br(),  # puts the next two logos on the next line
                                img(src = "HPVxNaviGA.png", height = "56px",
                                    style = "border:1px solid #e5e7eb; padding:6px; border-radius:8px; background:#fff;"),
                                img(src = "HPVxNaviNC.png", height = "56px",
                                    style = "border:1px solid #e5e7eb; padding:6px; border-radius:8px; background:#fff;")
                              ),
                              
                              # Title
                              div(style = "text-align:center; font-size:24px; font-weight:700; margin-bottom:10px;",
                                  "About HPVx Navigator: GA & NC"
                              ),
                              
                              # Body copy
                              div(style = "font-size:17px; line-height:1.7; margin-bottom:18px;",
                                  HTML(
                                    "HPVx Navigator helps individuals and families in Georgia and North Carolina find information about HPV vaccination eligibility, ",
                                    "nearby vaccine providers, and social support resources such as transportation, financial help, and language services. ",
                                    "This tool brings together publicly available, trusted resources and directs you to <b>official state webpages</b> and <b>2‑1‑1 directories</b>.<br><br>",
                                    "This tool was created as a part of the <b>ACS HPV Emerging Leaders Fellows Program's capstone project</b>. The purpose of this project is to increase access and uptake to HPV vaccination in North Carolina and Georgia. These two states are the beginning of a pilot program for this tool, they were selected due to proximity of developers and community need. Compared to the United States (5.2%), North Carolina and Georgia have higher percentages of Medicaid insured individuals. In North Carolina 41.3% of individuals have Medicaid while in Georgia 41.7% of individuals have Medicaid. The state of Georgia also has a higher uninsured population than the country at 6.3% and 5.3%, respectively. The intent of this tool is to roll out to further states after initial testing has been completed."
                                  )
                              ),
                              
                              # Developers  (REPLACES the old "Contacts" block)
                              tags$hr(),
                              div(style = "font-size:18px; font-weight:700; margin-bottom:10px;", "Developers"),
                              
                              fluidRow(
                                # --- Profile 1 ---
                                column(
                                  width = 6,
                                  div(style = "text-align:center; max-width:420px; margin:0 auto;",
                                      # Headshot
                                      img(
                                        src   = "ryan.jpg",
                                        height = "120px",
                                        style = "border:2px solid #e5e7eb; padding:6px; border-radius:100px; background:#fff; box-shadow:0 1px 6px rgba(0,0,0,.06);",
                                        alt   = "Headshot of developer"
                                      ),
                                      # Name
                                      div(style = "font-size:17px; font-weight:700; margin-top:10px;",
                                          "Ryan Suk, PhD"),
                                      # Bio (placeholder text; update as needed)
                                      div(style = "font-size:15px; line-height:1.6; color:#374151; margin-top:6px;",
                                          "Dashboard developer and Georgia content lead. Focuses on health economics, decision modeling, and decision support tools. (Short bio placeholder.)"),
                                      # Email
                                      div(style = "font-size:15px; margin-top:6px;",
                                          HTML("Email: <a href='mailto:ryan.suk@emory.edu'>ryan.suk@emory.edu</a>"))
                                  )
                                ),
                                
                                # --- Profile 2 ---
                                column(
                                  width = 6,
                                  div(style = "text-align:center; max-width:420px; margin:0 auto;",
                                      # Headshot
                                      img(
                                        src   = "rachael.jpg",
                                        height = "120px",
                                        style = "border:2px solid #e5e7eb; padding:6px; border-radius:100px; background:#fff; box-shadow:0 1px 6px rgba(0,0,0,.06);",
                                        alt   = "Headshot of developer"
                                      ),
                                      # Name
                                      div(style = "font-size:17px; font-weight:700; margin-top:10px;",
                                          "Rachael Baartmans, MPH"),
                                      # Bio (placeholder text; update as needed)
                                      div(style = "font-size:15px; line-height:1.6; color:#374151; margin-top:6px;",
                                          "Developer and North Carolina content lead. Specializes in public health program delivery and Vaccines for Children (VFC) program. (Short bio placeholder.)"),
                                      # Email
                                      div(style = "font-size:15px; margin-top:6px;",
                                          HTML("Email: <a href='mailto:rachael.baartmans@dhhs.nc.gov'>rachael.baartmans@dhhs.nc.gov</a>"))
                                  )
                                )
                              ),
                              div(style = "height:12px;"),  # small bottom spacing
                              
                              # Logos / partners  (REPLACE THIS WHOLE BLOCK)
                              tags$hr(),
                              div(style = "font-size:18px; font-weight:700; margin-bottom:10px;", "Partners"),
                              div(
                                style = "display:flex; gap:18px; align-items:center; flex-wrap:wrap; justify-content:center; margin-bottom:16px;",
                                
                                # HPV Roundtable — CLICKABLE LOGO
                                tags$a(
                                  href   = "https://hpvroundtable.org",  # <-- update if different
                                  target = "_blank",
                                  rel    = "noopener noreferrer",
                                  title  = "Visit HPV Vaccination Roundtable Site",
                                  img(
                                    src   = "HPVRT.png",
                                    height = "70px",
                                    style  = "border:1px solid #e5e7eb; padding:6px; border-radius:8px; background:#fff; cursor:pointer;",
                                    alt    = "HPV Roundtable logo"
                                  )
                                ),
                                
                                # GA HPV — CLICKABLE LOGO
                                tags$a(
                                  href   = "https://www.hpvcancerfreega.net/",  # <-- TODO: replace with the correct GA HPV site URL
                                  target = "_blank",
                                  rel    = "noopener noreferrer",
                                  title  = "Visit HPV Cancer Free GA site",
                                  img(
                                    src   = "gahpv.jpg",
                                    height = "70px",
                                    style  = "border:1px solid #e5e7eb; padding:6px; border-radius:8px; background:#fff; cursor:pointer;",
                                    alt    = "Georgia HPV site logo"
                                  )
                                )
                              ),
                              
                              # Disclaimers
                              tags$hr(),
                              div(style = "font-size:18px; font-weight:700; color:red; margin-bottom:6px;", "Important Notes"),
                              div(style = "font-size:15px; color:#374151; line-height:1.6;",
                                  HTML(
                                    "<b>Information only.</b> This site is for navigational and educational purposes and does not provide medical advice. ",
                                    "Please consult a qualified healthcare professional for personal medical questions.<br>",
                                    "<b>Privacy.</b> Inputs in this tool are used to generate on‑screen guidance and are not linked to your identity.<br>",
                                    "<b>Emergency.</b> If you are experiencing a medical emergency, call 911 immediately."
                                  )
                              ),
                              
                              # Footer spacing
                              div(style = "height:24px;"),
                              
                              # ---------- NEW: footer on this tab ---------- 
                              footerUI()
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
  
  # 🟢 NEW: Home button handler — reset app & go to first tab
  observeEvent(input$home_clicked, {
    # Reset the “clicked” state so the screener intro shows
    clicked(FALSE)
    # Go to the first tab
    updateTabsetPanel(session, "main_tabs", selected = "screening_tab")
    
    # Reset core inputs to their initial states
    updateSelectInput(session, "state", selected = "")
    updateNumericInput(session, "age", value = NA)
    updateSelectInput(session, "insurance", selected = "")
    # (optional) You can reset these as well if desired:
    # updateSelectInput(session, "insurance_type", selected = "Private")
    # updateSelectInput(session, "vaccine_coverage", selected = "Yes")
    
    updateSelectInput(session, "path_state", selected = "")
    updateTextInput(session, "zip_input", value = "")
    updateSelectInput(session, "transportation", selected = "")
    
    updateSelectInput(session, "resource_state", selected = "")
    updateCheckboxGroupInput(session, "needs", selected = character(0))
    
    # Refresh the Pathway Finder default panel content so it looks fresh next visit
    output$pathway_steps <- renderUI({
      HTML(paste0(
        "<div style=margin-top:10px;'>",
        "<img src='map.jpeg' style='height:350px; border:2px solid #e5e7eb; border-radius:10px; padding:6px; background:#fff; box-shadow:0 1px 6px rgba(0,0,0,.08);' alt='' onerror='this.onerror=null;this.src=\"placeholder.png\"'>",
        "<br><br>",
        "<div style='font-size:18px; line-height:1.6; text-align:left; margin:0 auto;'>",
        "<b>How to use this page:</b> Select your state, choose whether you need community resources, including transportation help, ",
        "and for Georgia enter your ZIP code. Then click <b>Show Steps</b> to see nearby providers (GA) ",
        "or step‑by‑step guidance (NC).",
        "</div>",
        "</div>"
      ))
    })
  })
  # -----------------------------
  
  
  # --- Eligibility Screener Logic ---
  output$eligibility_result <- renderUI({
    if (!clicked()) {
      return(HTML(" <div style='font-size:16px;'>
        <div style='font-size:18px; font-weight:bold;'>Welcome to HPVx Navigator State Focus Hub: GA & NC!</div><br>
        <b>HPVx Navigator Stat Focus Hub helps people in Georgia and North Carolina find free HPV vaccination (if eligible) and nearby social support resources—fast.</b><br><br>
        <b>Please complete the screener</b> (selection menu includes drop-down options and a short numeric field for age) to check your HPV vaccine eligibility.<br>
        Information you enter is anonymous and used only in aggregate and cannot be linked back to you.<br><br>
        <b>Contacts:</b><br>
        Ryan Suk, PhD 
        <a href='mailto:ryan.suk@emory.edu' style='color:blue; text-decoration:underline;'>
          ryan.suk@emory.edu
        </a> — For questions related to this dashboard and Georgia content<br>
        Rachael Baartmans, MPH 
        <a href='mailto:rachael.baartmans@dhhs.nc.gov' style='color:blue; text-decoration:underline;'>
          rachael.baartmans@dhhs.nc.gov
        </a> — For questions related to North Carolina content
      </div>
      <br><img src='infographic1.png' height='370px'
      style='border:2px solid #e5e7eb'>
      <br><br><div style='font-size:16px; font-weight:bold;'>IN PARTNERSHIP WITH..</div>
      <img src='HPVRT.png' height='80px'>
      <br><br><div style='font-size:16px; font-weight:bold;'>SHINY APP POWERED BY..</div>
      <img src='lab_logo.png' height='60px'><br>"))
    }
    
    message <- ""
    image <- ""
   
    # NEW: reusable “Not sure about insurance?” notice (injected for all age groups)
    not_sure_insurance_box <- if (!is.null(input$insurance) && input$insurance == "Not sure") {
      "<div style='background:#fff4e5; border:1px solid #f0ad4e; padding:10px; border-radius:6px; margin-bottom:12px;'>
    ❓ Not sure about insurance? Contact your employer or local public health department to check your insurance status.
    If you (or your child) have health insurance, HPV vaccination is likely covered up to age 45. For more details, please revisit this site to check your eligibility for specific programs.
    <br>
    <img src='timeline1.png' height='350px'
         style='display:block; margin:10px auto 0; border:1px solid #f0ad4e; border-radius:8px;'
         alt='Insurance information image'>
  </div>"
    } else { "" }
    
     if (input$state == "" || is.na(input$age) || input$insurance == "") {
      message <- "ℹ️ Please complete all required fields above before checking eligibility."
      image <- "placeholder.png"
    } else if (input$age < 19) {
      if (input$age < 9) {
        return(HTML(paste0(
          "<div style='font-size:18px;'>",
          "ℹ HPV vaccination is recommended starting at age 9. ",
          "Please check back when your child turns 9 and speak to a healthcare provider in ", input$state, " for guidance on HPV vaccination.",
          "</div><br>",
          "<img src='atnine.png' height='350px'><br><br>",
          "<img src='timeline1.png' height='230px'>"
        )))
        
      } else {  # age 9–18
        if (input$insurance == "No") {
          message <- paste("✅ You are eligible for free HPV vaccination through the VFC program in", input$state,
                           '. Check the <a href="#" onclick="$(\'a[data-value=\\\'pathway_tab\\\']\').tab(\'show\')" style="color:blue; font-size:18px; font-weight:bold; text-decoration:underline;">VFC Provider Finder</a>.')
          image <- "infographic2.png"
          
        } else if (input$insurance == "Yes") {
          if (!is.null(input$insurance_type) && input$insurance_type == "Medicaid") {
            message <- paste("✅ Since you have Medicaid and are under 19, you qualify for the VFC program in", input$state,
                             '. Check the <a href="#" onclick="$(\'a[data-value=\\\'pathway_tab\\\']\').tab(\'show\')" style="color:blue; font-size:18px; font-weight:bold; text-decoration:underline;">VFC Provider Finder</a>.')
            image <- "infographic2.png"
            
          } else if (!is.null(input$vaccine_coverage)) {
            if (input$vaccine_coverage == "No") {
              message <- paste("✅ Your insurance does not cover vaccines, so you may qualify for the VFC program in", input$state,
                               '. Check the <a href="#" onclick="$(\'a[data-value=\\\'pathway_tab\\\']\').tab(\'show\')" style="color:blue; font-size:18px; font-weight:bold; text-decoration:underline;">VFC Provider Finder</a>.')
              image <- "infographic2.png"
              
            } else if (input$vaccine_coverage == "Yes") {
              message <- paste("ℹ️ Since your insurance covers vaccines, you can get the HPV vaccine through your regular provider in", input$state)
              image <- "infographic2.png"
              
            } else {
              message <- paste("❓ If you're unsure about your vaccine coverage, contact your insurance provider or ask a clinic in", input$state,
                               ". You (or your child) may still be eligible for free vaccination through Vaccines for Children (VFC), if your insurance does not cover vaccines.")
              image <- "infographic2.png"
            }
            
          } else {
            message <- "ℹ️ Please answer all insurance-related questions to check eligibility."
            image <- "timeline1.png"
          }
        }
      }
    } else if (input$age >= 19) {
      if (input$age <= 26) {
        if (input$insurance == "No") {
          message <- paste("✅ You may be eligible for free or low-cost HPV vaccination through safety-net providers in", input$state,
                           ". HPV vaccination is recommended through age 26. Also, check your eligibility for a free vaccination program in ", input$state,
                           " through <a href='https://www.merckhelps.com/gardasil%209' target='_blank' style='color:blue; font-size:18px; font-weight:bold; text-decoration:underline;'>The Merck Patient Assistance Program</a>.")
        } else if (input$insurance == "Yes") {
          if (input$vaccine_coverage == "Yes") {
            message <- paste("✅ HPV vaccination is recommended through age 26. Your insurance should cover it— schedule with your provider in", input$state)
          } else if (input$vaccine_coverage == "No") {
            message <- paste("ℹ️ HPV vaccination is recommended through age 26. Since your insurance doesn’t cover vaccines, you may be eligible for free or low-cost HPV vaccination through safety-net providers in", input$state,
                             ". Also, check your eligibility for a free vaccination program in ", input$state,
                             " through <a href='https://www.merckhelps.com/gardasil%209' target='_blank' style='color:blue; font-size:18px; font-weight:bold; text-decoration:underline;'>The Merck Patient Assistance Program</a>.")
          } else {
            message <- paste("❓ HPV vaccination is recommended through age 26. Unsure about coverage? Ask your insurer or a provider in", input$state, "Please revisit this site for more details.")  
          }
        }
        
      } else if (input$age <= 45) {  # Ages 27–45
        if (input$insurance == "No") {
          message <- paste0(
            "ℹ️ HPV vaccination for ages 27–45 is based on shared clinical decision-making. If uninsured, check your eligibility for a free vaccination program in ", input$state,
            " through <a href='https://www.merckhelps.com/gardasil%209' target='_blank' style='color:blue; font-size:18px; font-weight:bold; text-decoration:underline;'>The Merck Patient Assistance Program</a>."
          )
        } else if (input$insurance == "Yes") {
          if (input$vaccine_coverage == "Yes") {
            message <- paste("✅ You are eligible for HPV vaccination based on shared decision-making. Your insurance likely covers it — ask your provider in", input$state)
          } else if (input$vaccine_coverage == "No") {
            message <- paste("ℹ️ You are eligible for HPV vaccination based on shared decision-making. Since your insurance doesn’t cover it, check your eligibility for a free vaccination program in ", input$state,
                             " through <a href='https://www.merckhelps.com/gardasil%209' target='_blank' style='color:blue; font-size:18px; font-weight:bold; text-decoration:underline;'>The Merck Patient Assistance Program</a>.")
          } else {
            message <- paste("❓You are eligible for HPV vaccination based on shared decision-making. Check with your insurer or provider in", input$state,
                             "to discuss coverage and need.")
          }
        }
        image <- "infographic3.jpg"
        
      } else {  # Over 45
        if (input$insurance == "No") {
          message <- paste("ℹ️ HPV vaccination is not recommended over age 45. If you're still interested, consult a provider in", input$state,
                           "for eligibility and cost options.")
        } else if (input$insurance == "Yes") {
          if (input$vaccine_coverage == "Yes") {
            message <- paste("✅HPV vaccination is not recoommended for ages over 45. Consult a provider in", input$state,
                             "about alternatives.")
          } else if (input$vaccine_coverage == "No") {
            message <- paste("ℹ️ HPV vaccination is not recommended over 45, and your insurance may not cover it. Ask a provider in", input$state,
                             "about alternatives.")
          } else {
            message <- paste("❓HPV vaccination is not recommended over 45. Consult a provider in", input$state,
                             "about alternatives.")
          }
        }
        image    <- "timeline1.png"  # your section-specific image
      }
    }
    
    
    # UPDATED: prepend the not-sure notice (if any) above the main message for all returns
    HTML(paste0(
      not_sure_insurance_box,  # NEW
      "<div style='font-size:16px;'>", message, "</div>",
      if (nzchar(image)) paste0(
        "<br><img src='", image, "' ",
        if (identical(image, "infographic3.jpg")) "height='700px'" else "height='350px'",
        ">"
      ) else ""
    ))
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
  
  # Default content when the tab first loads (before clicking "Show Steps")
  output$pathway_steps <- renderUI({
    HTML(paste0(
      "<div style=margin-top:10px;'>",
      "<img src='map.jpeg' style='height:350px; border:2px solid #e5e7eb; border-radius:10px; padding:6px; background:#fff; box-shadow:0 1px 6px rgba(0,0,0,.08);' alt='' onerror='this.onerror=null;this.src=\"placeholder.png\"'>",
      "<br><br>",
      "<div style='font-size:18px; line-height:1.6; text-align:left; margin:0 auto;'>",
      "<b>How to use this page:</b> Select your state, choose whether you need community resources, including transportation help, ",
      "and for Georgia enter your ZIP code. Then click <b>Show Steps</b> to see nearby providers (GA) ",
      "or step‑by‑step guidance (NC).",
      "</div>",
      "</div>"
    ))
  })
  
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
                "<a href='#' onclick=\"$('a[data-value=\\'resource_tab\\']').tab('show')\" style='color:blue; font-size:18px; font-weight:bold; text-decoration:underline;'>Community Resources</a> tab.<br><br>"
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
            "<a href='#' onclick=\"$('a[data-value=\\'resource_tab\\']').tab('show')\" style='color:blue; font-size:18px; font-weight:bold; text-decoration:underline;'>Community Resources</a> tab.<br><br>"
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
 
    # NEW: Show intro ONLY when no needs are selected
    if (is.null(input$needs) || length(input$needs) == 0) {
      links <- c(
        links,
        "<div style=margin-bottom:25px;'>",
        "<img src='resources.jpeg' height='250px' style='border-radius:10px;' alt='Community resources'>",
        "<br><br>",
        "<div style='font-size:18px; line-height:1.6; margin:0 auto;'>",
        "<b>Welcome to Communiry Resources tab!</b>",
        "<br><br>",
        "This tab directs you to trusted <b>state and community websites</b> that provide the social support resources you select in the menu ",
        "(e.g., transportation, financial help (including housing, food, utilities), and language support). ",
        "Links provided will redirect you to <b>official webpages</b>—such as state departments and <b>2‑1‑1</b> directories—for up‑to‑date programs and contact information.",
        "</div>",
        "</div>"
      )
      
      # 🔴 CHANGED: trigger scroll hint check after render (intro case)
      return(HTML(paste0(
        paste(links, collapse = ""),
        "<script>if(window.updateScrollHint){setTimeout(window.updateScrollHint,0);}</script>"
      )))
      
      
    }
    
    # --- Resource links by state ---
    if (input$resource_state == "Georgia") {
      if ("Transportation" %in% input$needs) {
        links <- c(
          links,
          "<br><br><img src='ga211trans.png' height='700px'>",
          "<br><br><a href='https://www.navigateresources.net/uwcg/search.aspx' target='_blank'><div style='font-size:18px; font-weight:bold;'>🚗 2-1-1 GA Resources Search Including Transportation (<-Click)</div></a> &mdash; Find local transit assistance, ride vouchers, and non‑emergency medical transport.<br>"
        )
      }
      if ("Financial Help" %in% input$needs) {
        # CHANGED: added short description after the link
        links <- c(
          links,
          "<br><br><img src='resources.jpeg' height='220px'>",
          "<br><br><a href='https://www.navigateresources.net/uwcg/search.aspx' target='_blank'><div style='font-size:18px; font-weight:bold;'>💰 2-1-1 GA Resources Search Including Financial Assistance</div></a> &mdash; Search for help with rent, utilities, and emergency bills.<br>"
        )
      }
      if ("Language Support" %in% input$needs) {
        links <- c(
          links,
          "<br><br><img src='language.jpeg' height='220px'>",
          # CHANGED: added short description after each link
          "<br><br><a href='https://dhs.georgia.gov/organization/about/language-access' target='_blank'><div style='font-size:18px; font-weight:bold;'>🗣️ Georgia Language Support Services</div></a> &mdash; State language access (interpreters and translated materials).<br>",
          "<br><a href='https://www.navigateresources.net/uwcg/search.aspx' target='_blank'><div style='font-size:18px; font-weight:bold;'>🌐 2-1-1 GA Resources Search Including Language Services</div></a> &mdash; Find interpreters, ESL, and translation services near you.<br>"
        )
      }
    } else if (input$resource_state == "North Carolina") {
      if ("Transportation" %in% input$needs) {
        # CHANGED: added short description after the link
        links <- c(
          links,
          "<br><br><img src='transport.jpeg' height='220px'>",
          "<br><br><a href='https://nc211.org/search/?keyword=transportation&location=NC++north+carolina&distance=100&skip=0&subtopic=Transportation&topic=Basic+Needs&taxonomyCode=' target='_blank'><div style='font-size:18px; font-weight:bold;'>🚗 NC Public Transportation</div></a> &mdash; Public and community ride options in your county.<br>"
        )
      }
      if ("Financial Help" %in% input$needs) {
        # CHANGED: added short description after the link
        links <- c(
          links,
          "<br><br><img src='resources.jpeg' height='220px'>",
          "<br><br><a href='https://nc211.org/search/?keyword=Income%20Support%20and%20Employment&location=NC%20%20north%20carolina&distance=100&skip=0&subtopic=Employment%2CPublic%20Assistance%20Programs%2CSocial%20Insurance%20Programs%2CTemporary%20Financial%20Assistance&topic=Income%20Support%20and%20Employment&taxonomyCode=' target='_blank'><div style='font-size:18px; font-weight:bold;'>💰 NC DHHS Financial Support</div></a> &mdash; Employment resources, public benefits, and temporary assistance for North Carolina residents.<br>",
          "<br><a href='https://www.consumerfinance.gov/housing/housing-insecurity/' target='_blank'><div style='font-size:18px; font-weight:bold;'>🏠 Consumer Financial Protection Bureau- Housing Insecurity</div></a> &mdash; US Government Agency that makes sure banks, lenders, and financial companies treat you fairly. Select from homeowner, renter, or housing lose to get more information. Each option includes a number of resources for every crisis.<br>"       
        )
      }
      if ("Language Support" %in% input$needs) {
        # CHANGED: added short description after the link
        links <- c(
          links,
          "<br><br><img src='language.jpeg' height='220px'>",
          "<br><br><a href='https://www.ncdhhs.gov/assistance/notice-informing-individuals-about-nondiscrimination-and-accessibility-requirements' target='_blank'><div style='font-size:18px; font-weight:bold;'>🗣️ NC Language Services</div></a> &mdash; Free interpreter services and accessibility information.<br>"
        )
      }
    }
    
    # 🔴 CHANGED: trigger scroll hint check after render (normal case)
    HTML(paste0(
      paste(links, collapse = ""),
      "<script>if(window.updateScrollHint){setTimeout(window.updateScrollHint,0);}</script>"
    ))
    
  })
  


}

# Run the app
shinyApp(ui = ui, server = server)
