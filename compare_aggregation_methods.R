## Show differences between weights
library(dplyr)
library(ggplot2)
library(patchwork)

#### ADM0
son_water_adm0 <- readRDS(here::here("/outputs", "adm_level", "sbtn_son_water_v1_1_adm0.rds"))

### Filter to only WP indicators
son_wp <- son_water_adm0 |> 
  select(NAME, GID_0, starts_with("wp"), cep_n:pgp_np)

son_wp_simple <- son_wp |> 
  st_transform("EPSG:8857") |> 
  st_simplify(dTolerance = 2500) |> 
  st_transform("EPSG:4326") |> 
  rowwise() |> 
  mutate(wp_max_diff = diff(c(wp_max_agr, wp_max_np)),
         cep_diff = diff(c(cep_agr, cep_np)),
         nox_diff = diff(c(nox_agr, nox_np)),
         pgp_diff = diff(c(pgp_agr, pgp_np)))

### TOTAL RISK (i.e. MAX)
total_map <- ggplot() +
  geom_sf(data = son_wp_simple,
          aes(fill = wp_max_diff),
          linewidth = 0.01) +
  coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in risk score\n(N&P risk - Cropland extent risk)") +
  labs(title = "Change in risk score\n(Maximum risk)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

total_hist <- ggplot() +
  geom_histogram(data = son_wp_simple,
                 aes(x = wp_max_diff), binwidth = 0.1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in risk score\n(N&P risk - Cropland extent risk)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_simple$wp_max_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

total_map/total_hist + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "total_risk_change.png"),
       width = 15, height = 10, dpi = 300)

ggplot() +
  geom_point(data = son_wp_simple,
             aes(x = wp_max_agr,
                 y = wp_max_np)) +
  geom_smooth(data = son_wp_simple,
              aes(x = wp_max_agr,
                  y = wp_max_np), method = "lm", se = FALSE,
              colour = "black", linewidth = 0.5) +
  geom_text(aes(x = 2, y = 4, label = paste0("Pearson's correlation:\n",
                                             round(cor(son_wp_simple$wp_max_agr,
                                                son_wp_simple$wp_max_np, use = "complete.obs"), 2)))) +
  theme_classic() +
  labs(x = "Total risk (Cropland extent)",
       y = "Total risk (N&P)")

ggsave(here::here("/outputs", "plots", "total_corr.png"),
       width = 5, height = 5, dpi = 300)

### COASTAL EUTROPHICATION
cep_map <- ggplot() +
  geom_sf(data = son_wp_simple,
          aes(fill = cep_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in risk score\n(N&P risk - Cropland extent risk)") +
  labs(title = "Change in risk score\n(Coastal Eutrophication Potential)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

cep_hist <- ggplot() +
  geom_histogram(data = son_wp_simple,
                 aes(x = cep_diff), binwidth = 0.1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in risk score\n(N&P risk - Cropland extent risk)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_simple$cep_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

cep_map/cep_hist + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "cep_risk_change.png"),
       width = 15, height = 10, dpi = 300)

ggplot() +
  geom_point(data = son_wp_simple,
             aes(x = cep_agr,
                 y = cep_np)) +
  geom_smooth(data = son_wp_simple,
              aes(x = cep_agr,
                  y = cep_np), method = "lm", se = FALSE,
              colour = "black", linewidth = 0.5) +
  geom_text(aes(x = 2, y = 4, label = paste0("Pearson's correlation:\n",
                                             round(cor(son_wp_simple$cep_agr,
                                                       son_wp_simple$cep_np, use = "complete.obs"), 2)))) +
  theme_classic() +
  labs(x = "Coastal Euthrophication risk (Cropland extent)",
       y = "Coastal Euthrophication risk (N&P)")

ggsave(here::here("/outputs", "plots", "cep_corr.png"),
       width = 5, height = 5, dpi = 300)

### Nox
nox_map <- ggplot() +
  geom_sf(data = son_wp_simple,
          aes(fill = nox_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in risk score\n(N&P risk - Cropland extent risk)") +
  labs(title = "Change in risk score\n(Nitrate-nitrite)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

nox_hist <- ggplot() +
  geom_histogram(data = son_wp_simple,
                 aes(x = nox_diff), binwidth = 0.1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in risk score\n(N&P risk - Cropland extent risk)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_simple$nox_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

nox_map/nox_hist + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "nox_risk_change.png"),
       width = 15, height = 10, dpi = 300)

ggplot() +
  geom_point(data = son_wp_simple,
             aes(x = nox_agr,
                 y = nox_np)) +
  geom_smooth(data = son_wp_simple,
              aes(x = nox_agr,
                  y = nox_np), method = "lm", se = FALSE,
              colour = "black", linewidth = 0.5) +
  geom_text(aes(x = 2, y = 4, label = paste0("Pearson's correlation:\n",
                                             round(cor(son_wp_simple$nox_agr,
                                                       son_wp_simple$nox_np, use = "complete.obs"), 2)))) +
  theme_classic() +
  labs(x = "Nitrate-Nitrite risk (Cropland extent)",
       y = "Nitrate-Nitrite risk (N&P)")

ggsave(here::here("/outputs", "plots", "nox_corr.png"),
       width = 5, height = 5, dpi = 300)

pgp_map <- ggplot() +
  geom_sf(data = son_wp_simple,
          aes(fill = pgp_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in risk score\n(N&P risk - Cropland extent risk)") +
  labs(title = "Change in risk score\n(Periphyton growth potential)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

pgp_hist <- ggplot() +
  geom_histogram(data = son_wp_simple,
                 aes(x = pgp_diff), binwidth = 0.1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in risk score\n(N&P risk - Cropland extent risk)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_simple$pgp_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

pgp_map/pgp_hist + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "pgp_risk_change.png"),
       width = 15, height = 10, dpi = 300)

ggplot() +
  geom_point(data = son_wp_simple,
             aes(x = pgp_agr,
                 y = pgp_np)) +
  geom_smooth(data = son_wp_simple,
              aes(x = pgp_agr,
                  y = pgp_np), method = "lm", se = FALSE,
              colour = "black", linewidth = 0.5) +
  geom_text(aes(x = 2, y = 4, label = paste0("Pearson's correlation:\n",
                                             round(cor(son_wp_simple$pgp_agr,
                                                       son_wp_simple$pgp_np, use = "complete.obs"), 2)))) +
  theme_classic() +
  labs(x = "Periphyton growth risk (Cropland extent)",
       y = "Periphyton growth risk (N&P)")

ggsave(here::here("/outputs", "plots", "pgp_corr.png"),
       width = 5, height = 5, dpi = 300)

## Rank them
son_wp_rank <- son_water_adm0 |> 
  select(GID_0, starts_with("wp"), cep_n:pgp_np) |> 
  mutate_if(is.numeric, dense_rank) |> 
  rowwise() |> 
  mutate(wp_max_diff = diff(c(wp_max_agr, wp_max_np)),
         cep_diff = diff(c(cep_agr, cep_np)),
         nox_diff = diff(c(nox_agr, nox_np)),
         pgp_diff = diff(c(pgp_agr, pgp_np)))

son_wp_rank_simple <- son_wp_rank |> 
  st_transform("EPSG:8857") |> 
  st_simplify(dTolerance = 2500) |> 
  st_transform("EPSG:4326")

total_map_rank <- ggplot() +
  geom_sf(data = son_wp_rank_simple,
          aes(fill = wp_max_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in country rank\n(N&P rank - Cropland extent rank)") +
  labs(title = "Change in country rank\n(Maximum risk)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

total_hist_rank <- ggplot() +
  geom_histogram(data = son_wp_rank_simple,
                 aes(x = wp_max_diff), binwidth = 1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in risk score\n(N&P rank - Cropland extent rank)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_rank_simple$wp_max_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

total_map_rank/total_hist_rank + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "total_rank_change.png"),
       width = 15, height = 10, dpi = 300)

### COASTAL EUTROPHICATION
cep_map_rank <- ggplot() +
  geom_sf(data = son_wp_rank_simple,
          aes(fill = cep_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in country rank\n(N&P rank - Cropland extent rank)") +
  labs(title = "Change in country rank\n(Coastal Eutrophication Potential)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

cep_hist_rank <- ggplot() +
  geom_histogram(data = son_wp_rank_simple,
                 aes(x = cep_diff), binwidth = 1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in country rank\n(N&P rank - Cropland extent rank)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_rank_simple$cep_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

cep_map_rank/cep_hist_rank + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "cep_rank_change.png"),
       width = 15, height = 10, dpi = 300)

### Nox
nox_map_rank <- ggplot() +
  geom_sf(data = son_wp_rank_simple,
          aes(fill = nox_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in country rank\n(N&P rank - Cropland extent rank)") +
  labs(title = "Change in country rank\n(Nitrate-nitrite)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

nox_hist_rank <- ggplot() +
  geom_histogram(data = son_wp_rank_simple,
                 aes(x = nox_diff), binwidth = 1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in country rank\n(N&P rank - Cropland extent rank)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_rank_simple$nox_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

nox_map_rank/nox_hist_rank + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "nox_rank_change.png"),
       width = 15, height = 10, dpi = 300)

pgp_map_rank <- ggplot() +
  geom_sf(data = son_wp_rank_simple,
          aes(fill = pgp_diff),
          linewidth = 0.01) +   coord_sf(ylim = c(-65, 80)) +
  scale_fill_gradient2(low = "#006837", mid = "white", high = "#a50026",
                       name = "Change in country rank\n(N&P rank - Cropland extent rank)") +
  labs(title = "Change in country rank\n(Periphyton growth potential)") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               theme = theme(legend.key.width = unit(5, "cm"))))

pgp_hist_rank <- ggplot() +
  geom_histogram(data = son_wp_rank_simple,
                 aes(x = pgp_diff), binwidth = 1,
                 colour = "black", fill = "grey80") +
  labs(
    # title = "Change in risk score\n(Maximum risk)",
    x = "Change in country rank\n(N&P rank - Cropland extent rank)",
    subtitle = paste0("Mean change: ", round(mean(son_wp_rank_simple$pgp_diff, na.rm = TRUE), 2))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

pgp_map_rank/pgp_hist_rank + patchwork::plot_layout(heights = c(0.75, 0.25))

ggsave(here::here("/outputs", "plots", "pgp_rank_change.png"),
       width = 15, height = 10, dpi = 300)

### Output as csv
output <- son_wp |> 
  left_join(son_wp_rank |> 
              st_drop_geometry() |> 
              select(-contains("diff")) |> 
              rename_at(.vars = vars(-GID_0), .funs = \(x) paste0(x, "_rank"))) |> 
  select(NAME, GID_0, starts_with("wp"), starts_with("cep"), starts_with("nox"), starts_with("pgp")) |> 
  st_drop_geometry()

write.csv(output, here::here("/outputs", "plots", "aggregation_comparison.csv"), row.names = FALSE)
