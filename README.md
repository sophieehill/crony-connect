# What is Crony Connect?

Crony Connect is a Shiny app that allows you to find companies associated with an individual (using Companies House) and then search for that individual and their associated companies in the register of political donations and the register of MP's financial interests. The goal is to help researchers and journalists identify networks of political influence by partially automating the process of cross-referencing between these commonly used databases.

While Companies House has a nice API, the two political registers do not and so I took the very inelegant approach of downloading the whole datasets and storing them locally (!). 

This app was previously live at [cronyconnect.com](https://cronyconnect.com/) but has been taken down while I figure out how to make it financially sustainable.

# How to run locally

1. Clone the repo
2. Open the downloaded .Rproj file in RStudio
3. Click "Run app" (or, equivalently, `runApp("app")`)
