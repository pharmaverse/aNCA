## LLM example

# Make sure you have these packages installed
# install.packages("ggplot2")
# install.packages("aNCA") # Assuming this is where filter_breaks is from

library(ggplot2)
library(aNCA) # Load the package containing filter_breaks

# --- 1. Create a very simple ggplot2 histogram ---
# We'll use mtcars$mpg for simplicity, similar to your hist(mtcars) idea
# Let's explicitly set many breaks to see the filtering effect

# Define a wide range of breaks for demonstration
# From min(mpg) to max(mpg) by small increments
initial_breaks <- seq(floor(min(mtcars$mpg)), ceiling(max(mtcars$mpg)), by = 0.5)
initial_breaks

p <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  scale_x_continuous(breaks = initial_breaks) + # Apply our initial dense breaks
  labs(title = "MPG Distribution (Before filter_breaks)") +
  theme_minimal()

# --- 2. View the plot *before* filter_breaks ---
print(p)

# --- 3. Use filter_breaks() to get the filtered breaks ---
# We'll use a min_cm_distance of 1 cm for a noticeable effect

## temporary workaround
filtered_breaks <-initial_breaks

## will throw error
filtered_breaks <- filter_breaks(
  breaks = initial_breaks,
  plot = p,
  min_cm_distance = 1, # You can adjust this value
  axis = "x"
)

# --- 4. Create the plot *after* filter_breaks with the new breaks ---
p_filtered <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  scale_x_continuous(breaks = filtered_breaks) + # Apply the filtered breaks
  labs(title = paste0("MPG Distribution (After filter_breaks, min_cm_distance = ", 1, "cm)")) +
  theme_minimal()

# --- 5. View the filtered plot ---
print(p_filtered)

# You can also inspect the breaks:
cat("\nInitial breaks (first 10):", head(initial_breaks, 10), "...\n")
cat("Number of initial breaks:", length(initial_breaks), "\n")
cat("\nFiltered breaks (first 10):", head(filtered_breaks, 10), "...\n")
cat("Number of filtered breaks:", length(filtered_breaks), "\n")
