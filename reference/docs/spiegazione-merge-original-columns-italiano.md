# Funzionamento della Funzione merge_original_columns()

## Il Problema

La funzione vecshift trasforma dati di contratti lavorativi in una sequenza di segmenti temporali continui. Durante questa trasformazione, però, vengono mantenute solo le informazioni essenziali per il calcolo temporale: identificativi, date e stati occupazionali. Informazioni aggiuntive presenti nei dati originali, come il nome dell'azienda, il salario, il reparto di lavoro o altre caratteristiche del contratto, vengono perdute nel processo di trasformazione.

La funzione merge_original_columns risolve questo problema arricchendo i segmenti temporali prodotti da vecshift con informazioni supplementari provenienti dai dati originali dei contratti.

## La Soluzione

La funzione merge_original_columns esegue un collegamento tra i segmenti temporali e i dati originali dei contratti utilizzando l'identificativo univoco (colonna 'id'). Questo processo permette di trasferire qualsiasi colonna dai dati originali ai segmenti temporali trasformati, mantenendo la coerenza temporale e l'integrità dei dati.

## Come Funziona

### Fase 1: Validazione dei Dati di Input

Il sistema verifica che tutti gli elementi necessari siano presenti e corretti:

- I dati originali e i segmenti devono essere in formato data.table
- Entrambi i dataset devono contenere una colonna 'id' per il collegamento
- Le colonne richieste per l'unione devono esistere nei dati originali
- Il sistema identifica eventuali conflitti di nomi tra le colonne

### Fase 2: Preparazione dei Dati per l'Unione

La funzione separa i segmenti in due categorie:

**Segmenti di Occupazione (id > 0)**: Corrispondono a periodi di lavoro derivati da contratti specifici. Questi segmenti possono essere collegati ai dati originali tramite l'identificativo del contratto.

**Segmenti di Disoccupazione (id = 0)**: Rappresentano periodi senza contratti attivi. Non hanno corrispondenza nei dati originali poiché non derivano da contratti specifici.

### Fase 3: Unione delle Informazioni

Per i segmenti di occupazione, il sistema esegue un'unione basata sull'identificativo:

```
segmenti con id=1 → collegati al contratto originale con id=1
segmenti con id=2 → collegati al contratto originale con id=2
```

Questo collegamento permette di trasferire tutte le informazioni aggiuntive del contratto originale ai segmenti temporali corrispondenti.

### Fase 4: Gestione dei Periodi di Disoccupazione

Per i segmenti di disoccupazione, la funzione aggiunge le colonne richieste con valori mancanti (NA). Il tipo di valore mancante viene determinato dal tipo di dato della colonna originale:

- Colonne di testo: NA_character_
- Colonne numeriche: NA_real_
- Colonne di date: as.Date(NA)
- Colonne logiche: NA

### Fase 5: Ricomposizione e Ordinamento

I segmenti di occupazione arricchiti e quelli di disoccupazione vengono ricombinati in un unico dataset. Il risultato finale viene ordinato per persona (cf) e data di inizio (inizio) per mantenere la sequenza temporale corretta.

## Esempio Pratico

### Dati Originali dei Contratti
```
id | cf       | INIZIO     | FINE       | prior | azienda   | salario | reparto
1  | ABC123   | 2023-01-01 | 2023-05-31 | 1     | AziendaA  | 50000   | IT
2  | ABC123   | 2023-08-01 | 2023-12-31 | 0     | AziendaB  | 30000   | HR
3  | DEF456   | 2023-02-01 | 2023-11-30 | 1     | AziendaC  | 60000   | Finance
```

### Segmenti Prodotti da vecshift
```
cf     | inizio     | fine       | id | arco | stato
ABC123 | 2023-01-01 | 2023-05-31 | 1  | 1    | occ_ft
ABC123 | 2023-06-01 | 2023-07-31 | 0  | 0    | disoccupato  
ABC123 | 2023-08-01 | 2023-12-31 | 2  | 1    | occ_pt
DEF456 | 2023-02-01 | 2023-11-30 | 3  | 1    | occ_ft
```

### Applicazione di merge_original_columns
```r
risultato <- merge_original_columns(
  original_data = dati_originali,
  segments = segmenti,
  columns = c("azienda", "salario", "reparto")
)
```

### Risultato Arricchito
```
cf     | inizio     | fine       | id | arco | stato       | azienda  | salario | reparto
ABC123 | 2023-01-01 | 2023-05-31 | 1  | 1    | occ_ft      | AziendaA | 50000   | IT
ABC123 | 2023-06-01 | 2023-07-31 | 0  | 0    | disoccupato | NA       | NA      | NA
ABC123 | 2023-08-01 | 2023-12-31 | 2  | 1    | occ_pt      | AziendaB | 30000   | HR
DEF456 | 2023-02-01 | 2023-11-30 | 3  | 1    | occ_ft      | AziendaC | 60000   | Finance
```

## Logica di Unione

### Principio di Corrispondenza
Il collegamento tra segmenti e dati originali avviene attraverso l'identificativo del contratto. Poiché vecshift può suddividere un singolo contratto in più segmenti temporali (ad esempio, quando ci sono sovrapposizioni con altri contratti), le stesse informazioni del contratto originale possono apparire in più righe del risultato finale.

### Duplicazione delle Informazioni
Quando un contratto viene suddiviso in più segmenti, le informazioni aggiuntive vengono replicate:

```
Contratto originale: id=1, azienda="AziendaA", salario=50000
Segmento 1: id=1, periodo gen-mar, azienda="AziendaA", salario=50000
Segmento 2: id=1, periodo apr-mag, azienda="AziendaA", salario=50000
```

Questa duplicazione è corretta dal punto di vista logico: ogni segmento temporale mantiene le caratteristiche del contratto da cui deriva.

### Gestione delle Sovrapposizioni
Quando più contratti si sovrappongono temporalmente, ogni segmento mantiene le informazioni del proprio contratto di origine. Per gestire le informazioni combinate nei periodi di sovrapposizione, è disponibile la funzione merge_overlapping_values che applica regole specifiche per tipo di dato.

## Struttura dell'Output

### Colonne Preservate
Il risultato mantiene tutte le colonne originali dei segmenti prodotti da vecshift:
- Identificativi (cf, id)
- Date (inizio, fine, durata)
- Stati occupazionali (arco, stato)

### Colonne Aggiunte
Le colonne specificate nel parametro vengono aggiunte con le informazioni dei contratti originali. L'ordine delle colonne nel risultato segue la struttura: colonne dei segmenti + colonne aggiunte.

### Ordinamento Temporale
Il risultato finale mantiene l'ordinamento temporale per persona, essenziale per analisi longitudinali e visualizzazioni cronologiche.

## Casi Speciali e Considerazioni

### Segmenti di Disoccupazione
I periodi di disoccupazione (id = 0) rappresentano intervalli temporali senza contratti attivi. Per questi segmenti, le colonne aggiunte contengono valori mancanti appropriati al tipo di dato. Questa gestione permette di mantenere la continuità temporale senza introdurre informazioni spurie.

### Contratti Sovrapposti
In presenza di contratti sovrapposti, ogni segmento temporale mantiene le informazioni del contratto da cui deriva. Il sistema non combina automaticamente le informazioni dei contratti sovrapposti: questa operazione può essere eseguita successivamente con merge_overlapping_values.

### Valori Mancanti nei Dati Originali
Se i dati originali contengono valori mancanti nelle colonne da unire, questi vengono preservati nei segmenti corrispondenti. Il sistema distingue tra valori mancanti originali e valori mancanti inseriti per i periodi di disoccupazione.

### Conflitti di Nomi di Colonna
Quando una colonna specificata per l'unione ha lo stesso nome di una colonna esistente nei segmenti, la funzione emette un avvertimento e la colonna dai dati originali sovrascrive quella nei segmenti. L'unica eccezione è la colonna 'id', utilizzata come chiave di unione.

### Segmenti non Abbinabili
Se esistono segmenti con identificativi che non corrispondono a contratti nei dati originali, la funzione emette un avvertimento. Questi segmenti mantengono valori mancanti per le colonne richieste.

## Cosa Significa Questo per Voi

La funzione merge_original_columns trasforma i risultati tecnici di vecshift in dataset informativi completi per l'analisi. Permette di mantenere il contesto dei contratti originali durante l'analisi temporale, facilitando:

- Analisi di carriera con informazioni su aziende e ruoli
- Calcoli di reddito aggregato considerando tutti i contratti
- Analisi di mobilità lavorativa tra settori o aziende
- Reporting dettagliato che combina aspetti temporali e caratteristiche contrattuali

Il risultato è un dataset che conserva la precisione temporale della trasformazione vecshift arricchendola con tutte le informazioni di contesto necessarie per analisi complete e significative.