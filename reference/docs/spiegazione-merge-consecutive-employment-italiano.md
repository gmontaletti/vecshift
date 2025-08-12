# Funzione merge_consecutive_employment_fast(): Consolidamento Periodi di Impiego Consecutivi

## Il Problema

Quando si analizzano le carriere lavorative attraverso la funzione `vecshift()`, i dati risultanti contengono segmenti temporali dettagliati che mostrano ogni singolo periodo di impiego e disoccupazione. Tuttavia, per molte analisi è necessario consolidare i periodi di impiego consecutivi in segmenti unici, mantenendo separati solo i periodi di disoccupazione che rappresentano interruzioni significative nella carriera.

La funzione `merge_consecutive_employment_fast()` risolve questo problema identificando e consolidando automaticamente i periodi di impiego consecutivi, semplificando la struttura dei dati mantenendo l'accuratezza delle informazioni temporali.

## La Soluzione

La funzione `merge_consecutive_employment_fast()` rappresenta un'implementazione ottimizzata per il consolidamento dei periodi di impiego. Applica una logica basata su eventi, simile a quella utilizzata da `vecshift()`, per identificare e raggruppare i segmenti temporali consecutivi dove sussiste un rapporto di lavoro attivo (arco > 0).

La versione "fast" utilizza un approccio semplificato rispetto alla versione standard, concentrandosi sulla performance attraverso operazioni di raggruppamento più efficienti.

## Dati di Input

La funzione elabora l'output della funzione `vecshift()`, che consiste in segmenti temporali con le seguenti caratteristiche:

### Colonne Obbligatorie
- **cf**: Identificativo della persona (codice fiscale)
- **inizio**: Data di inizio del segmento temporale
- **fine**: Data di fine del segmento temporale
- **arco**: Numero di contratti sovrapposti (0 = disoccupazione, >0 = impiego)
- **prior**: Indicatore del tipo di impiego (numerico)

### Colonne Aggiuntive
- **Colonne numeriche**: Parametri contrattuali come salario, orario di lavoro
- **Colonne carattere**: Informazioni qualitative come tipo contratto, settore

## Come Funziona

### Fase 1: Identificazione dello Status di Impiego

La funzione inizia classificando ogni segmento come periodo di impiego o disoccupazione:

```
employment_status = ifelse(arco > 0, 1, 0)
```

Ogni segmento viene etichettato con 1 (impiego) o 0 (disoccupazione) basandosi sul valore di `arco`.

### Fase 2: Rilevamento delle Transizioni

Utilizza la funzione `shift()` per confrontare lo status di ogni segmento con quello precedente:

```
status_change = employment_status != shift(employment_status, 1L)
```

Questa operazione identifica i punti di transizione tra impiego e disoccupazione.

### Fase 3: Raggruppamento Preliminare

Crea gruppi preliminari usando `cumsum()` sulle transizioni di status:

```
group_id = cumsum(status_change), by = cf
```

Ogni transizione genera un nuovo identificativo di gruppo per persona.

### Fase 4: Verifica della Consecutività

Per i periodi di impiego, verifica l'assenza di gap temporali tra segmenti consecutivi:

```
has_gap = employment_status == 1 & (inizio - prev_fine) > 1
```

Un gap esistente indica che due periodi di impiego non sono realmente consecutivi.

### Fase 5: Raggruppamento Finale

Combina le informazioni di status e consecutività per creare il raggruppamento definitivo:

```
subgroup_id = cumsum(has_gap | status_change), by = cf
```

Questo genera l'identificativo finale per ogni gruppo di segmenti da consolidare.

### Fase 6: Aggregazione dei Dati

Per ogni gruppo, la funzione calcola:

**Date di consolidamento:**
- Data inizio: prima data del gruppo
- Data fine: ultima data del gruppo

**Valori numerici (medie ponderate per durata):**
```
weighted.mean(valori, pesi = durata_segmenti)
```

**Valori carattere (concatenazione):**
- Valore singolo: mantenuto invariato
- Valori multipli diversi: "primo->ultimo"

## Esempio Pratico

### Dati di Input
```
cf    inizio      fine        arco  prior  salario  tipo_contratto
A1    2023-01-01  2023-02-28   1     1.0   3000     "Determinato"
A1    2023-03-01  2023-04-30   1     1.0   3100     "Determinato" 
A1    2023-05-01  2023-05-15   0     0.0   NA       NA
A1    2023-05-16  2023-12-31   1     0.5   2500     "Indeterminato"
```

### Analisi del Consolidamento

**Segmenti 1-2**: Periodi di impiego consecutivi (fine primo = inizio secondo - 1)
**Segmento 3**: Periodo di disoccupazione (arco = 0)
**Segmento 4**: Nuovo periodo di impiego dopo gap di disoccupazione

### Output Consolidato
```
cf    inizio      fine        arco  prior  salario  tipo_contratto         durata  collapsed
A1    2023-01-01  2023-04-30   1.0   1.0   3067     "Determinato"         119     TRUE
A1    2023-05-01  2023-05-15   0     0.0   NA       NA                    15      FALSE
A1    2023-05-16  2023-12-31   1     0.5   2500     "Indeterminato"       230     FALSE
```

### Calcoli delle Medie Ponderate

Per il periodo consolidato (01/01 - 30/04):
- Durata segmento 1: 59 giorni
- Durata segmento 2: 60 giorni
- Salario medio: (3000×59 + 3100×60) / (59+60) = 3067

## Logica di Consolidamento

### Criteri per il Raggruppamento

**Periodi di Impiego Consecutivi:**
- Stesso codice fiscale (cf)
- Entrambi con arco > 0
- Assenza di gap temporali (fine + 1 = inizio successivo)

**Periodi Non Consolidati:**
- Periodi di disoccupazione (arco = 0)
- Periodi di impiego isolati
- Periodi separati da gap di disoccupazione

### Gestione delle Colonne Aggiuntive

**Colonne Numeriche:**
- Media ponderata per durata del segmento
- Colonna "_direction": differenza tra ultimo e primo valore
- Arrotondamento a 2 decimali

**Colonne Carattere:**
- Valore unico: mantenuto invariato
- Valori diversi: concatenazione "primo->ultimo"
- Valori mancanti: gestiti appropriatamente

## Output della Funzione

### Struttura del Risultato

Il data.table risultante contiene:

**Colonne Base:**
- **cf, inizio, fine**: Identificativo e confini temporali
- **arco**: Media ponderata per periodi consolidati, valore originale per altri
- **prior**: Media ponderata del tipo di impiego
- **durata**: Durata totale del periodo consolidato

**Colonne Derivate:**
- **collapsed**: Indicatore logico (TRUE = periodo consolidato)
- **n_periods**: Numero di segmenti originali consolidati
- **[colonna]_direction**: Direzione di cambiamento per valori numerici

**Colonne Aggiuntive:**
- Medie ponderate per colonne numeriche originali
- Concatenazioni per colonne carattere originali

### Significato della Colonna "collapsed"

- **TRUE**: Il periodo risulta dal consolidamento di più segmenti consecutivi
- **FALSE**: Il periodo corrisponde a un singolo segmento originale

Questa informazione permette di distinguere i periodi effettivamente consolidati da quelli mantenuti nella forma originale.

## Casi Speciali e Considerazioni

### Periodi Singoli
I segmenti che non hanno periodi consecutivi adiacenti vengono mantenuti inalterati, con `collapsed = FALSE`.

### Contratti Sovrapposti Consecutivi
Quando più contratti sono attivi simultaneamente in periodi consecutivi, i valori di `arco` vengono consolidati tramite media ponderata, preservando l'informazione sull'intensità occupazionale.

### Gestione Valori Mancanti
- Valori NA nelle colonne numeriche: esclusi dal calcolo delle medie
- Stringhe vuote nelle colonne carattere: trattate come valori mancanti
- Record con tutti i valori mancanti: preservati con NA nel risultato

### Differenze con la Versione Standard

La versione `_fast` ottimizza le performance attraverso:

**Raggruppamento Semplificato:**
- Utilizza operazioni di shift più efficienti
- Riduce il numero di passaggi di elaborazione
- Mantiene la stessa logica di consolidamento

**Gestione Memoria:**
- Meno colonne temporanee create durante l'elaborazione
- Aggregazioni più dirette senza cicli complessi

**Prestazioni:**
- Tempi di esecuzione ridotti su dataset di grandi dimensioni
- Minore consumo di memoria durante l'elaborazione

## Cosa Significa Questo per Voi

La funzione `merge_consecutive_employment_fast()` trasforma dati occupazionali dettagliati in una rappresentazione semplificata che mantiene le informazioni essenziali eliminando la frammentazione artificiosa dei periodi di impiego.

**Benefici per l'Analisi:**
- Riduzione della complessità dei dati mantenendo l'accuratezza temporale
- Identificazione chiara dei periodi di carriera significativi
- Calcoli statistici più affidabili su periodi occupazionali reali
- Visualizzazioni più leggibili delle traiettorie di carriera

**Applicazioni Pratiche:**
- Analisi della stabilità occupazionale
- Calcolo della durata media dei rapporti di lavoro
- Identificazione di pattern di mobilità professionale
- Valutazioni per politiche del lavoro e supporto all'occupazione

La funzione preserva l'integrità temporale dei dati originali consolidando intelligentemente solo i periodi effettivamente consecutivi, fornendo una base solida per analisi quantitative e qualitative delle carriere lavorative.