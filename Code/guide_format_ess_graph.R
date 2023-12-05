library(ggplot2)

ggplot(Data, aes(x = guide_format_ess_test, )) +
  geom_bar(stat = "identity", position = "dodge")
