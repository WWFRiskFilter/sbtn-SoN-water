## Build all unified layers
pacman::p_load(here, quarto)

all_files <- c(
  here("v2.0", "sbtn-SoN-water_v2.0.qmd"),
  here("v2.1", "sbtn-SoN-water_v2.1.qmd"),
  here("v1.1", "sbtn-SoN-water_v1.1.qmd")
)

for(qmd_path in all_files){
  
  quarto::quarto_render(qmd_path)
  
}