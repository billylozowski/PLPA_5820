[→ Noel et al. (2022) article
←](https://doi.org/10.1094/PDIS-06-21-1253-RE)

### DON (ppm) x Treatment

``` r
p1 <- ggplot(mycotoxin, aes(Treatment, DON, fill = Cultivar)) +       # set plot mapping
  geom_boxplot(position = position_dodge(0.85), outlier.shape = NA) + # dodge the boxplots + hide outliers
  geom_jitter(shape = 21, width = 0.3, alpha = 0.6) +                 # set point shape, dodge width, and transparency
  scale_fill_manual(values = c(cbbPalette[(3)], cbbPalette[(4)])) +   # specify fill colours
  labs(x = "",                                                        # define labels
       y = "DON (ppm)") +
  theme_classic() +
  facet_wrap(~Cultivar)                                               # separate plots by Cultivar

P1 <- p1 + 
  stat_compare_means(method = "anova") +  
  geom_pwc(aes(group = Treatment),  # specify which means you want to compare
               method = "t.test",
               label = "{p.adj.format}{p.adj.signif}")
P1
```

![](Coding-Challenge-4_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### X15ADON x Treatment

``` r
p3 <- ggplot(mycotoxin, aes(Treatment, X15ADON, fill = Cultivar)) +          # mapping
  geom_boxplot(position = position_dodge(0.85), outlier.shape = NA) +        # boxplot mapping
  geom_jitter(shape = 21, width = 0.3, alpha = 0.6) +                        # jitter mapping
  scale_fill_manual(values = c(cbbPalette[(3)], cbbPalette[(4)])) +          # fill colours
  labs(x = "",                                                               # labels
       y = "15ADON") +
  theme_classic() +
  facet_wrap(~Cultivar)                                                      # separate plots

P3 <- p3 +
  stat_compare_means(method = "anova") +  
  geom_pwc(aes(group = Treatment),  # specify which means you want to compare
               method = "t.test",
               label = "{p.adj.format}{p.adj.signif}")
P3
```

![](Coding-Challenge-4_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Seed Mass x Treatment

``` r
p4 <- ggplot(mycotoxin, aes(Treatment, MassperSeed_mg, fill = Cultivar)) +   # mapping
  geom_boxplot(position = position_dodge(0.85), outlier.shape = NA) +        # boxplot mapping
  geom_jitter(shape = 21, width = 0.3, alpha = 0.6) +                        # jitter mapping
  scale_fill_manual(values = c(cbbPalette[(3)], cbbPalette[(4)])) +          # fill colours
  labs(x = "",                                                               # labels
       y = "Seed Mass (mg)") +
  theme_classic() +
  facet_wrap(~Cultivar)                                                      # separate plots

P4 <- p4 +
  stat_compare_means(method = "anova") +  
  geom_pwc(aes(group = Treatment),  # specify which means you want to compare
               method = "t.test",
               label = "{p.adj.format}{p.adj.signif}")
P4
```

![](Coding-Challenge-4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Combined plot with ‘ggarrange’

``` r
# arrange plots + stats in combined figure
fig2 <- ggarrange(
  P1, P3, P4,        # select plot objects
  labels = "auto",
  nrow = 1,
  ncol = 3,
  legend = FALSE     # remove legend
)
fig2
```

![](Coding-Challenge-4_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
