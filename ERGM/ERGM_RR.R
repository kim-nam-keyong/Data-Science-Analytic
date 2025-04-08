rm(list=ls())
library(intergraph)
library(parallel)
library(network)
library(ergm)
library(sna)
library(readr)
library(igraph)
library(RColorBrewer)
library(intergraph)

elec_corr24 <- read_csv("./elec_corr.csv",show_col_types = FALSE)
chemi_corr24 <- read_csv("./chemi_corr.csv",show_col_types = FALSE)
elec_corr23 <- read_csv("./elec_corr23.csv",show_col_types = FALSE)
chemi_corr23 <- read_csv("./chemi_corr23.csv",show_col_types = FALSE)


df_elec24 <- read_csv("./elec.csv",show_col_types = FALSE) 
df_chemi24 <- read_csv("./chemi.csv",show_col_types = FALSE)
df_elec23 <- read_csv("./elec_23.csv",show_col_types = FALSE)
df_chemi23 <- read_csv("./chemi_23.csv",show_col_types = FALSE)

df_elec24 <- df_elec24[order(df_elec24$종목명), ] 
df_chemi24 <- df_chemi24[order(df_chemi24$종목명), ]
df_elec23 <- df_elec23[order(df_elec23$종목명),]
df_chemi23 <- df_chemi23[order(df_chemi23$종목명), ]

# 임계값 설정
threshold <- 0.7

# 전기전자 네트워크 구축
elec_adj_matrix24 <- abs(as.matrix(elec_corr24)) > threshold
elec_net24 <- graph_from_adjacency_matrix(elec_adj_matrix24, mode = "undirected", diag = FALSE)
elec_adj_matrix23 <- abs(as.matrix(elec_corr23)) > threshold
elec_net23 <- graph_from_adjacency_matrix(elec_adj_matrix23, mode = 'undirected', diag = FALSE)

# 화학 네트워크 구축
chemi_adj_matrix24 <- abs(as.matrix(chemi_corr24)) > threshold
chemi_net24 <- graph_from_adjacency_matrix(chemi_adj_matrix24, mode = "undirected", diag = FALSE)
chemi_adj_matrix23 <- abs(as.matrix(chemi_corr23)) > threshold
chemi_net23 <- graph_from_adjacency_matrix(chemi_adj_matrix23, mode = "undirected", diag = FALSE)


# 전기전자 네트워크 노드 속성 추가
V(elec_net24)$기업명 <- df_elec24$종목명
V(elec_net24)$시가총액 <- df_elec24$시가총액
V(elec_net24)$등락치 <- df_elec24$등락치
V(elec_net24)$거래대금_일평균 <- df_elec24$거래대금_일평균
V(elec_net24)$거래대금_합계 <- df_elec24$거래대금_합계
V(elec_net24)$종가 <- df_elec24$종가
### 2023 
V(elec_net23)$기업명 <- df_elec23$종목명
V(elec_net23)$시가총액 <- df_elec23$시가총액
V(elec_net23)$등락치 <- df_elec23$등락치
V(elec_net23)$거래대금_일평균 <- df_elec23$거래대금_일평균
V(elec_net23)$거래대금_합계 <- df_elec23$거래대금_합계
V(elec_net23)$종가 <- df_elec23$종가



# 화학 네트워크 노드 속성 추가
V(chemi_net24)$기업명 <- df_chemi24$종목명
V(chemi_net24)$시가총액 <- df_chemi24$시가총액
V(chemi_net24)$등락치 <- df_chemi24$등락치
V(chemi_net24)$거래대금_일평균 <- df_chemi24$거래대금_일평균
V(chemi_net24)$거래대금_합계 <- df_chemi24$거래대금_합계
V(chemi_net24)$종가 <- df_chemi24$종가
### 2023 
V(chemi_net23)$기업명 <- df_chemi23$종목명
V(chemi_net23)$시가총액 <- df_chemi23$시가총액
V(chemi_net23)$등락치 <- df_chemi23$등락치
V(chemi_net23)$거래대금_일평균 <- df_chemi23$거래대금_일평균
V(chemi_net23)$거래대금_합계 <- df_chemi23$거래대금_합계
V(chemi_net23)$종가 <- df_chemi23$종가



###############################
#전기전자 네트워크 통계 분석 24
elec_density24 <- edge_density(elec_net24)
elec_average_degree24 <- mean(degree(elec_net24))
elec_transitivity24 <- transitivity(elec_net24, type = "global")
elec_clustering_coefficient24 <- transitivity(elec_net24, type = "average")
###############################
#전기전자 네트워크 통계 분석 23
elec_density23 <- edge_density(elec_net23)
elec_average_degree23 <- mean(degree(elec_net23))
elec_transitivity23 <- transitivity(elec_net23, type = "global")
elec_clustering_coefficient23 <- transitivity(elec_net23, type = "average")

cat("Electronics Network Density23:", elec_density23, "\n")
cat("Electronics Average Degree23:", elec_average_degree23, "\n")
cat("Electronics Transitivity23:", elec_transitivity23, "\n")
cat("Electronics Clustering Coefficient23:", elec_clustering_coefficient23, "\n")

cat("Electronics Network Density24:", elec_density24, "\n")
cat("Electronics Average Degree24:", elec_average_degree24, "\n")
cat("Electronics Transitivity24:", elec_transitivity24, "\n")
cat("Electronics Clustering Coefficient24:", elec_clustering_coefficient24, "\n")


###############################
# 화학 네트워크 통계 분석
chemi_density23 <- edge_density(chemi_net23)
chemi_average_degree23 <- mean(degree(chemi_net23))
chemi_transitivity23 <- transitivity(chemi_net23, type = "global")
chemi_clustering_coefficient23 <- transitivity(chemi_net23, type = "average")

chemi_density24 <- edge_density(chemi_net24)
chemi_average_degree24 <- mean(degree(chemi_net24))
chemi_transitivity24 <- transitivity(chemi_net24, type = "global")
chemi_clustering_coefficient24 <- transitivity(chemi_net24, type = "average")


cat("Chemical Network Density23:", chemi_density23, "\n")
cat("Chemical Average Degree23:", chemi_average_degree23, "\n")
cat("Chemical Transitivity23:", chemi_transitivity23, "\n")
cat("Chemical Clustering Coefficient23:", chemi_clustering_coefficient23, "\n")

cat("Chemical Network Density24:", chemi_density24, "\n")
cat("Chemical Average Degree24:", chemi_average_degree24, "\n")
cat("Chemical Transitivity24:", chemi_transitivity24, "\n")
cat("Chemical Clustering Coefficient24:", chemi_clustering_coefficient24, "\n")

# 중심성 분석 함수
centrality_analysis <- function(net, net_name) {
  degree_centrality <- degree(net)
  closeness_centrality <- closeness(net)
  betweenness_centrality <- betweenness(net)
  
  cat(paste(net_name, "Network Centrality:\n"))
  cat("Degree Centrality:\n")
  print(summary(degree_centrality))
  cat("Closeness Centrality:\n")
  print(summary(closeness_centrality))
  cat("Betweenness Centrality:\n")
  print(summary(betweenness_centrality))
  
  return(list(degree = degree_centrality, closeness = closeness_centrality, betweenness = betweenness_centrality))
}

# 전기전자 네트워크 중심성 분석
elec_centrality_23 <- centrality_analysis(elec_net23, "Electronics 2023")
elec_centrality_24 <- centrality_analysis(elec_net24, "Electronics 2024")

# 화학 네트워크 중심성 분석
chemi_centrality_23 <- centrality_analysis(chemi_net23, "Chemical 2023")
chemi_centrality_24 <- centrality_analysis(chemi_net24, "Chemical 2024")


# 중심성 지표 상위 10개 노드 출력 함수
print_top_10_centralities <- function(centrality, title) {
  top_10 <- sort(centrality, decreasing = TRUE)[1:10]
  cat(title, "Top 10 Nodes:\n")
  print(top_10)
}

# 전기전자 네트워크 상위 10개 노드 출력
cat("Electronics 2023 Degree Centrality:\n")
print_top_10_centralities(elec_centrality_23$degree, "Degree Centrality")
cat("Electronics 2023 Closeness Centrality:\n")
print_top_10_centralities(elec_centrality_23$closeness, "Closeness Centrality")
cat("Electronics 2023 Betweenness Centrality:\n")
print_top_10_centralities(elec_centrality_23$betweenness, "Betweenness Centrality")

cat("Electronics 2024 Degree Centrality:\n")
print_top_10_centralities(elec_centrality_24$degree, "Degree Centrality")
cat("Electronics 2024 Closeness Centrality:\n")
print_top_10_centralities(elec_centrality_24$closeness, "Closeness Centrality")
cat("Electronics 2024 Betweenness Centrality:\n")
print_top_10_centralities(elec_centrality_24$betweenness, "Betweenness Centrality")

# 화학 네트워크 상위 10개 노드 출력
cat("Chemical 2023 Degree Centrality:\n")
print_top_10_centralities(chemi_centrality_23$degree, "Degree Centrality")
cat("Chemical 2023 Closeness Centrality:\n")
print_top_10_centralities(chemi_centrality_23$closeness, "Closeness Centrality")
cat("Chemical 2023 Betweenness Centrality:\n")
print_top_10_centralities(chemi_centrality_23$betweenness, "Betweenness Centrality")

cat("Chemical 2024 Degree Centrality:\n")
print_top_10_centralities(chemi_centrality_24$degree, "Degree Centrality")
cat("Chemical 2024 Closeness Centrality:\n")
print_top_10_centralities(chemi_centrality_24$closeness, "Closeness Centrality")
cat("Chemical 2024 Betweenness Centrality:\n")
print_top_10_centralities(chemi_centrality_24$betweenness, "Betweenness Centrality")



##########################################################################


# 모듈성 및 커뮤니티 구조 분석 함수
community_analysis <- function(net, net_name) {
  communities <- cluster_fast_greedy(net)
  
  cat(paste(net_name, "Network Communities:\n"))
  print(sizes(communities))
  cat("Modularity:", modularity(communities), "\n")
  
  return(communities)
}

# 전기전자 네트워크 커뮤니티 분석
elec_communities_23 <- community_analysis(elec_net23, "Electronics 2023")
elec_communities_24 <- community_analysis(elec_net24, "Electronics 2024")

# 화학 네트워크 커뮤니티 분석
chemi_communities_23 <- community_analysis(chemi_net23, "Chemical 2023")
chemi_communities_24 <- community_analysis(chemi_net24, "Chemical 2024")




# 커뮤니티 탐지 및 기업명 출력 함수
print_community_companies <- function(net, net_name) {
  community_detection <- cluster_fast_greedy(net)
  membership <- membership(community_detection)
  company_communities <- data.frame(Company = V(net)$기업명, Community = membership)
  
  cat(paste(net_name, "Network Communities:\n"))
  for (i in 1:max(membership)) {
    cat(paste("Community", i, ":\n"))
    print(company_communities$Company[company_communities$Community == i])
    cat("\n")
  }
}

# 전기전자 2023 네트워크
print_community_companies(elec_net23, "Electronics 2023")

# 전기전자 2024 네트워크
print_community_companies(elec_net24, "Electronics 2024")

# 화학 2023 네트워크
print_community_companies(chemi_net23, "Chemical 2023")

# 화학 2024 네트워크
print_community_companies(chemi_net24, "Chemical 2024")



######################################
# 구조적 공백 분석 함수 
# 구조적 공백 분석 함수
structural_holes_analysis <- function(net, net_name) {
  holes <- constraint(net)
  
  cat(paste(net_name, "Network Structural Holes:\n"))
  print(summary(holes))
  
  return(holes)
}

# 전기전자 네트워크 구조적 공백 분석
elec_holes_23 <- structural_holes_analysis(elec_net23, "Electronics 2023")
elec_holes_24 <- structural_holes_analysis(elec_net24, "Electronics 2024")

# 화학 네트워크 구조적 공백 분석
chemi_holes_23 <- structural_holes_analysis(chemi_net23, "Chemical 2023")
chemi_holes_24 <- structural_holes_analysis(chemi_net24, "Chemical 2024")
#####


# 구조적 공백 값 계산 함수 (결측값 제거 포함)
calculate_structural_holes <- function(net) {
  constraint <- constraint(net)
  constraint <- constraint[!is.na(constraint)]  # 결측값 제거
  return(constraint)
}

# 구조적 공백 분석 및 노드 식별 함수
analyze_structural_holes <- function(net, net_name) {
  structural_holes <- calculate_structural_holes(net)
  low_holes_nodes <- names(structural_holes)[structural_holes < quantile(structural_holes, 0.25, na.rm = TRUE)]
  high_holes_nodes <- names(structural_holes)[structural_holes > quantile(structural_holes, 0.75, na.rm = TRUE)]
  
  cat(paste(net_name, "Network Structural Holes Analysis:\n"))
  cat("Nodes with Low Structural Holes:\n")
  print(low_holes_nodes)
  cat("Nodes with High Structural Holes:\n")
  print(high_holes_nodes)
  
  return(list(low = low_holes_nodes, high = high_holes_nodes))
}

# 전기전자 2023 네트워크 구조적 공백 분석
elec_sholes_23 <- analyze_structural_holes(elec_net23, "Electronics 2023")

# 전기전자 2024 네트워크 구조적 공백 분석
elec_sholes_24 <- analyze_structural_holes(elec_net24, "Electronics 2024")

# 화학 2023 네트워크 구조적 공백 분석
chemi_sholes_23 <- analyze_structural_holes(chemi_net23, "Chemical 2023")

# 화학 2024 네트워크 구조적 공백 분석
chemi_sholes_24 <- analyze_structural_holes(chemi_net24, "Chemical 2024")

###################################################################
# 변화율 계산 함수
calculate_change_rate <- function(stat_2023, stat_2024) {
  change_rate <- (stat_2024 - stat_2023) / stat_2023
  return(change_rate)
}

# 전기전자 네트워크 변화율 계산
elec_density_change <- calculate_change_rate(elec_stats_23$density, elec_stats_24$density)
elec_avg_degree_change <- calculate_change_rate(elec_stats_23$avg_degree, elec_stats_24$avg_degree)
elec_transitivity_global_change <- calculate_change_rate(elec_stats_23$transitivity_global, elec_stats_24$transitivity_global)
elec_transitivity_avg_change <- calculate_change_rate(elec_stats_23$transitivity_avg, elec_stats_24$transitivity_avg)

# 화학 네트워크 변화율 계산
chemi_density_change <- calculate_change_rate(chemi_stats_23$density, chemi_stats_24$density)
chemi_avg_degree_change <- calculate_change_rate(chemi_stats_23$avg_degree, chemi_stats_24$avg_degree)
chemi_transitivity_global_change <- calculate_change_rate(chemi_stats_23$transitivity_global, chemi_stats_24$transitivity_global)
chemi_transitivity_avg_change <- calculate_change_rate(chemi_stats_23$transitivity_avg, chemi_stats_24$transitivity_avg)

# 변화율 출력
cat("Electronics Network Change Rates:\n")
cat("Density Change Rate:", elec_density_change, "\n")
cat("Average Degree Change Rate:", elec_avg_degree_change, "\n")
cat("Global Transitivity Change Rate:", elec_transitivity_global_change, "\n")
cat("Average Clustering Coefficient Change Rate:", elec_transitivity_avg_change, "\n")

cat("\nChemical Network Change Rates:\n")
cat("Density Change Rate:", chemi_density_change, "\n")
cat("Average Degree Change Rate:", chemi_avg_degree_change, "\n")
cat("Global Transitivity Change Rate:", chemi_transitivity_global_change, "\n")
cat("Average Clustering Coefficient Change Rate:", chemi_transitivity_avg_change, "\n")

###################################################################
# 특정 기업의 위치 변화 분석 함수
analyze_position_change <- function(net_2023, net_2024, company_name) {
  degree_2023 <- degree(net_2023)[company_name]
  degree_2024 <- degree(net_2024)[company_name]
  
  closeness_2023 <- closeness(net_2023)[company_name]
  closeness_2024 <- closeness(net_2024)[company_name]
  
  betweenness_2023 <- betweenness(net_2023)[company_name]
  betweenness_2024 <- betweenness(net_2024)[company_name]
  
  cat(paste("Position Change for", company_name, ":\n"))
  cat("Degree Centrality Change:", degree_2024 - degree_2023, "\n")
  cat("Closeness Centrality Change:", closeness_2024 - closeness_2023, "\n")
  cat("Betweenness Centrality Change:", betweenness_2024 - betweenness_2023, "\n")
}

# 특정 기업의 위치 변화 분석 (예: 특정 기업 이름 사용)
analyze_position_change(elec_net23, elec_net24, "삼성전자")
analyze_position_change(chemi_net23, chemi_net24, "LG화학")
analyze_position_change(elec_net23, elec_net24, "SK하이닉스")
analyze_position_change(chemi_net23, chemi_net24, "SK이노베이션")


############################################

# igraph 객체를 network 객체로 변환
elec_net23_network <- asNetwork(elec_net23)
elec_net24_network <- asNetwork(elec_net24)
chemi_net23_network <- asNetwork(chemi_net23)
chemi_net24_network <- asNetwork(chemi_net24)

# 노드 속성 추가 함수
add_node_attributes <- function(net, df) {
  net %v% "업종" <- df$업종명
  net %v% "거래대금_일평균" <- df$거래대금_일평균
  net %v% "거래대금_합계" <- df$거래대금_합계
  net %v% "종가" <- df$종가
  net %v% "시가총액" <- df$시가총액
  net %v% "등락치평균" <- df$등락치
  return(net)
}

# 전기전자 네트워크 노드 속성 추가
elec_net23_network <- add_node_attributes(elec_net23_network, df_elec23)
elec_net24_network <- add_node_attributes(elec_net24_network, df_elec24)

# 화학 네트워크 노드 속성 추가
chemi_net23_network <- add_node_attributes(chemi_net23_network, df_chemi23)
chemi_net24_network <- add_node_attributes(chemi_net24_network, df_chemi24)

# ERGM 모델 적합 함수
fit_ergm <- function(net, net_name) {
  ergm_model <- ergm(net ~ edges + nodematch("업종") + nodecov("거래대금_일평균") +
                       nodecov("거래대금_합계") + nodecov("종가") +
                       nodecov("시가총액") + absdiff("등락치평균"),
                     control = control.ergm(MCMLE.maxit = 100, MCMC.burnin = 20000, MCMC.interval = 2000))
  
  cat(paste(net_name, "ERGM Model Summary:\n"))
  print(summary(ergm_model))
  
  return(ergm_model)
}

# 전기전자 네트워크 ERGM 모델 적합
ergm_model_elec23 <- fit_ergm(elec_net23_network, "Electronics 2023")
ergm_model_elec24 <- fit_ergm(elec_net24_network, "Electronics 2024")

# 화학 네트워크 ERGM 모델 적합
ergm_model_chemi23 <- fit_ergm(chemi_net23_network, "Chemical 2023")
ergm_model_chemi24 <- fit_ergm(chemi_net24_network, "Chemical 2024")


#####################################################################
# 네트워크 시각화 함수
plot_network <- function(net, net_name, top_10_names) {
  vertex_color <- ifelse(V(net)$name %in% top_10_names, "red", "blue")
  vertex_size <- ifelse(V(net)$name %in% top_10_names, 10, 5)
  layout <- layout_with_fr(net)
  
  plot(net, layout = layout, vertex.size = vertex_size, vertex.label.cex = 0.7, vertex.color = vertex_color,
       edge.color = "gray", edge.width = 1, main = paste(net_name, "Network (Top 10 Nodes Highlighted)"))
}

# 전기전자 네트워크에서 상위 10개 노드 추출
top_10_elec23 <- order(degree(elec_net23), decreasing = TRUE)[1:10]
top_10_elec24 <- order(degree(elec_net24), decreasing = TRUE)[1:10]
top_10_elec_names23 <- V(elec_net23)$name[top_10_elec23]
top_10_elec_names24 <- V(elec_net24)$name[top_10_elec24]

# 화학 네트워크에서 상위 10개 노드 추출
top_10_chemi23 <- order(degree(chemi_net23), decreasing = TRUE)[1:10]
top_10_chemi24 <- order(degree(chemi_net24), decreasing = TRUE)[1:10]
top_10_chemi_names23 <- V(chemi_net23)$name[top_10_chemi23]
top_10_chemi_names24 <- V(chemi_net24)$name[top_10_chemi24]

# 전기전자 네트워크 시각화
plot_network(elec_net23, "Electronics 2023", top_10_elec_names23)
plot_network(elec_net24, "Electronics 2024", top_10_elec_names24)

# 화학 네트워크 시각화
plot_network(chemi_net23, "Chemical 2023", top_10_chemi_names23)
plot_network(chemi_net24, "Chemical 2024", top_10_chemi_names24)
  
  
  
  
print(top_10_elec_names23)
print(top_10_elec_names24)
print(top_10_chemi_names23)
print(top_10_chemi_names24)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

#############################################################
# test version
  
  
palette <- brewer.pal(9, "YlOrRd") 
node_degree <- degree(elec_net)
node_colors <- palette[cut(node_degree, breaks = 9, labels = FALSE)]

# 전기전자 네트워크 시각화
elec_업종_색상 <- setNames(c("blue", "red"), unique(df_elec$업종명))
elec_vertex_color <- elec_업종_색상[V(elec_net)$업종]
elec_vertex_size <- log(V(elec_net)$시가총액 + 1) / 5
elec_layout <- layout_with_fr(elec_net)

plot(elec_net, layout = elec_layout, vertex.size = elec_vertex_size, vertex.label.cex = 0.7, vertex.color = elec_vertex_color,
     edge.color = "gray", edge.width = 1, main = "Electronics Network")

# 화학 네트워크 시각화
chemi_업종_색상 <- setNames(c("blue", "red"), unique(df_chemi$업종명))
chemi_vertex_color <- chemi_업종_색상[V(chemi_net)$업종]
chemi_vertex_size <- log(V(chemi_net)$시가총액 + 1) / 5
chemi_layout <- layout_with_fr(chemi_net)

plot(chemi_net, layout = chemi_layout, vertex.size = chemi_vertex_size, vertex.label.cex = 0.7, vertex.color = chemi_vertex_color,
     edge.color = "gray", edge.width = 1, main = "Chemical Network")

# igraph 객체를 network 객체로 변환
elec_net <- asNetwork(elec_net)
chemi_net <- asNetwork(chemi_net)

# 인기성 및 활동성 (무방향 그래프의 경우)
degree_centrality <- degree(elec_net, mode = "all")
# 사슬성
two_paths <- count_two_paths(elec_net)
# 이행성 (추가적인 분석)
triad_census <- triad.census(elec_net)


# ERGM 모델 생성
ergm_model_elec <- ergm(elec_net ~ edges + nodematch("업종") + nodecov("거래대금_일평균") +
                          nodecov("거래대금_합계")+nodecov("종가")+
                          nodecov("시가총액") + absdiff("등락치평균"),
                          control = control.ergm(MCMLE.maxit = 100, MCMC.burnin = 20000, MCMC.interval = 2000))

ergm_model_chemi <- ergm(chemi_net ~ edges + nodematch("업종") + nodecov("거래대금_일평균") +
                           nodecov("거래대금_합계")+nodecov("종가")+
                           nodecov("시가총액") + absdiff("등락치평균"),
                         control = control.ergm(MCMLE.maxit = 100, MCMC.burnin = 20000, MCMC.interval = 2000))

# 모델 요약 출력 (전기전자)
summary(ergm_model_elec)

# 모델 요약 출력 (화학)
summary(ergm_model_chemi)


# network 객체를 igraph 객체로 변환
elec_net <- asIgraph(elec_net)
chemi_net <- asIgraph(chemi_net)

# 노드 이름 설정 (예: 종목명 사용)
V(elec_net)$name <- df_elec$종목명
V(chemi_net)$name <- df_chemi$종목명

# 전기전자 네트워크에서 상위 10개 노드 추출
elec_degree <- degree(elec_net)
top_10_elec <- order(elec_degree, decreasing = TRUE)[1:10]
top_10_elec_names <- V(elec_net)$name[top_10_elec]

# 화학 네트워크에서 상위 10개 노드 추출
chemi_degree <- degree(chemi_net)
top_10_chemi <- order(chemi_degree, decreasing = TRUE)[1:10]
top_10_chemi_names <- V(chemi_net)$name[top_10_chemi]

# 상위 10개 노드 출력
cat("Top 10 Electronics Companies by Degree:\n")
print(top_10_elec_names)

cat("Top 10 Chemical Companies by Degree:\n")
print(top_10_chemi_names)

# 전기전자 네트워크 시각화 (상위 10개 노드 강조)
elec_vertex_color <- ifelse(V(elec_net)$name %in% top_10_elec_names, "red", "blue")
elec_vertex_size <- ifelse(V(elec_net)$name %in% top_10_elec_names, 10, 5)
elec_layout <- layout_with_fr(elec_net)

plot(elec_net, layout = elec_layout, vertex.size = elec_vertex_size, vertex.label.cex = 0.7, vertex.color = elec_vertex_color,
     edge.color = "gray", edge.width = 1, main = "Electronics Network (Top 10 Nodes Highlighted)")

# 화학 네트워크 시각화 (상위 10개 노드 강조)
chemi_vertex_color <- ifelse(V(chemi_net)$name %in% top_10_chemi_names, "red", "blue")
chemi_vertex_size <- ifelse(V(chemi_net)$name %in% top_10_chemi_names, 10, 5)
chemi_layout <- layout_with_fr(chemi_net)

plot(chemi_net, layout = chemi_layout, vertex.size = chemi_vertex_size, vertex.label.cex = 0.7, vertex.color = chemi_vertex_color,
     edge.color = "gray", edge.width = 1, main = "Chemical Network (Top 10 Nodes Highlighted)")

