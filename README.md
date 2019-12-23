# 🤵 문재인 대통령 연설에 대한 Dendrogram 

- 목표: 5개 대통령 연설에 대해 Dendrogram 만들기

- 과정: 
1. 연설문 5개 불러와서 하나의 데이터로 합치기
2. 함수 만들어주기
3. TDM 형태로 만들기
4. 클러스터링 분석하기
5. Dendrogram 만들기

- 코드:
<pre><code># 필요한 패키지 설치하기
install.packages("tm")
install.packages("KoNLP")
install.packages("stringr")
install.packages("NbClust")
install.packages("devtools")

library(devtools)
install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)

# 필요한 패키지 불러오기
library(tm)
library(KoNLP)
library(stringr)
library(NbClust)

# 사전 불러오기
useNIADic() # 안되는 경우 useSejongDic()

# 데이터 불러오기
data1 <- readLines("연설문1.txt", encoding = "UTF-8")
data2 <- readLines("연설문2.txt", encoding = "UTF-8")
data3 <- readLines("연설문3.txt", encoding = "UTF-8")
data4 <- readLines("연설문4.txt", encoding = "UTF-8")
data5 <- readLines("연설문5.txt", encoding = "UTF-8")

# 데이터 하나로 합치기
data <- rbind(data1, data2, data3, data4, data5)

# 함수 만들어주기
ko.words <- function(doc) {
  d <- as.character(doc)
  extractNoun(d)
}
  
# 단어-문서 행렬 만들어주기
cps <- VCorpus(VectorSource(data))
tdm <- TermDocumentMatrix(cps, control = list(tokenize = ko.words,
                                              removePunctuation = TRUE,
                                              removeNumbers = TRUE,
                                              wordLengths = c(2, Inf),
                                              stopwords = c("여러분", "들이", "오늘",
                                                            "내용", "주제", "공판")))
  
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix)) <- "UTF-8" # UTF-8 오류시 CP949로 변경
  
word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing = TRUE)
freq.words <- tdm.matrix[word.order[1:30],] # 30개만 추출하였습니다.
co.matrix <- freq.words %*% t(freq.words)

# 클러스터링 분석하여 Dendrogram 그리기
dist.co.matrix <- dist(t(co.matrix), method = "euclidean")
fit <- hclust(dist.co.matrix)
plot(fit, main = "문재인 대통령 연설문 분석", cex.main = 2, lwd = 1, lty = 1, hang = -1,
     cex = 1.1, col = "#333333", col.main = "#363636", col.lab = "#363636", col.axis = "#AA0002")</code></pre>
     
- 결과물 예시:
<p><img src="https://github.com/draxcel/korean_president_speech/blob/master/%EA%B2%B0%EA%B3%BC%EB%AC%BC.png?raw=true" alt="최종 결과물" width="650" height="550"></p>
