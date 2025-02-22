# Analisi esplorativa del mercato immobiliare del Texas

## **Descrizione e obiettivi del progetto**  
Questo progetto, realizzato durante il Master in Data Science di ProfessionAI, consiste nell'effettuare un'analisi esplorativa del mercato immobiliare del Texas utilizzando il linguaggio **R**. L'obiettivo è fornire all'azienda immaginaria *Texas Realty Insights* insight statistici e visivi utili per prendere decisioni strategiche basate su dati concreti, sfruttando i dati storici delle vendite immobiliari. In particolare si vogliono identificare e interpretare i trend storici delle vendite, valutare l’efficacia delle strategie di marketing delle inserzioni immobiliari e offrire una rappresentazione grafica dei dati che evidenzi la distribuzione dei prezzi e delle vendite tra città, mesi e anni.

Il progetto include:  
- Un file `.R` contenente tutto il codice eseguibile.  
- Un documento `PDF` che spiega nel dettaglio ogni passaggio dell'analisi.  
- Il dataset utilizzato (`Real Estate Texas.csv`).

Il dataset è costituito da 240 osservazioni delle seguenti variabili: `city`, `year`, `month`, `sales`, `volume`, `median_price`, `listings` e `months_inventory`.

## **Analisi effettuate**  
- Esplorazione delle variabili e calcolo di indici statistici (posizione, variabilità, forma).
- Identificazione delle variabili con maggiore variabilità e asimmetria.
- Suddivisione del **prezzo mediano di vendita** in classi, costruzione della distribuzione di frequenza e calcolo dell'indice di Gini.  
- Creazione di nuove variabili utili all'analisi: **prezzo medio di vendita** e **tasso di conversione degli annunci di vendita**.  
- Utilizzo del pacchetto `dplyr` per generare summary condizionati a città, anni e mesi.  
- Visualizzazioni grafiche avanzate con `ggplot2`, tra cui: boxplot, grafici a barre e line chart.
 
## **Tecnologie utilizzate**  
- **Linguaggio**: R
- **Ambiente di sviluppo**: RStudio  
- **Pacchetti principali**:  
  - `dplyr` (manipolazione dati).  
  - `ggplot2` (visualizzazione dati).
  - `moments` (calcolo di asimmetria e curtosi).

## **Come eseguire l'analisi**  
1. Scarica o clona il repository.
2. Apri il file `analisi_mercato_immobiliare_texas.R` in RStudio o un altro ambiente R.
3. Esegui il codice passo-passo per ottenere i risultati dell'analisi e le visualizzazioni grafiche.
4. Il dataset utilizzato (`Real Estate Texas.csv`) è incluso nel repository. Assicurati che il file si trovi nella stessa directory del file .R.

## Autore
[Virginio Cocciaglia](https://github.com/VirginioC)

---
