# Week 8: Pre-lab, Lab, Problem Set
# Lecture Videos

# 2. Researchers are interested in whether or not the average person consumes 2,000 calories per day. Their random sample of 25 people consumed an average of 1,891 calories, with a standard deviation of 251 calories.

# 2a. What is the t-statistic? (Report to 2 decimal places.)
round((1891-2000)/(251/sqrt(25)),2)

# 2b. What is the absolute critical t value, assuming ??=0.05?
round(qt(.975,24),2)

# 3. Scientists fear that polar bears are slowly starving due to their shrinking habitat. A healthy male polar bear weighs about 900 pounds. A new expedition was able to estimate the weight of 7 male polar bears. They found an average weight of 861 lbs with a standard deviation of 59 pounds.

# 3c. What is the value of the standard error? (Report to 1 decimal place.)
round(59/sqrt(7),1)

# 3d. What is the t-statistic? (Report to 3 decimal places.)
round((861-900)/(59/sqrt(7)),3)

# 3e. What is the t-critical value, assuming ??=0.05?
-qt(.95,6)
