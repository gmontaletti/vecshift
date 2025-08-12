# Spiegazione della Funzione vecshift: Analisi Temporale dei Dati di Impiego

## Il Problema

Immaginate di dover analizzare la storia lavorativa di migliaia di persone. Avete a disposizione i dati dei singoli contratti - quando iniziano, quando finiscono, se sono a tempo pieno o parziale - ma quello che serve davvero è comprendere la situazione lavorativa complessiva di ogni persona in ogni momento nel tempo.

Il problema è complesso: le persone possono avere periodi di disoccupazione tra un contratto e l'altro, oppure possono avere più contratti contemporaneamente (lavori sovrapposti). Come trasformare una lista di contratti individuali in una sequenza temporale continua che mostra chiaramente tutti i periodi della vita lavorativa di una persona?

Questo è esattamente il problema che risolve la funzione `vecshift`.

## La Soluzione

La funzione `vecshift` trasforma i dati dei contratti di lavoro in segmenti temporali continui che descrivono completamente la situazione lavorativa di ogni persona nel tempo. È come costruire una linea temporale dettagliata che non lascia mai vuoti o zone d'ombra.

**Cosa otteniamo:**
- **Periodi di disoccupazione**: Quando la persona non ha contratti attivi
- **Periodi di occupazione singola**: Quando la persona ha un solo contratto attivo
- **Periodi di occupazione sovrapposta**: Quando la persona ha più contratti contemporaneamente
- **Durata precisa**: Per ogni periodo, la durata esatta in giorni

## Come Funziona

### Struttura dei Dati di Input

La funzione richiede una tabella con queste colonne obbligatorie:

| Colonna | Significato | Esempio |
|---------|-------------|---------|
| `id` | Identificativo univoco del contratto | 1, 2, 3, ... |
| `cf` | Codice fiscale della persona | "RSSMRA80A01H501X" |
| `INIZIO` | Data di inizio contratto (inclusa) | 2023-01-01 |
| `FINE` | Data di fine contratto (inclusa) | 2023-06-30 |
| `prior` | Tipo di impiego (0=part-time, >0=full-time) | 0, 1 |

**Esempio di dati di input:**
```
id  cf      INIZIO      FINE        prior
1   A001    2023-01-01  2023-03-31  1     (contratto full-time)
2   A001    2023-06-01  2023-12-31  0     (contratto part-time)
3   B002    2023-02-01  2023-11-30  1     (contratto full-time)
```

### Il Processo Passo dopo Passo

#### Passo 1: Trasformazione in Eventi

Ogni contratto viene scomposto in due eventi:
- **Evento di inizio**: Alla data `INIZIO`, viene aggiunto +1 (inizia un contratto)
- **Evento di fine**: Alla data `FINE`, viene aggiunto -1 (finisce un contratto)

**Esempio:**
```
Contratto: INIZIO=2023-01-01, FINE=2023-03-31
Diventa:
- 2023-01-01: +1 (evento di inizio)
- 2023-03-31: -1 (evento di fine)
```

#### Passo 2: Calcolo delle Sovrapposizioni

Gli eventi vengono ordinati cronologicamente e si calcola la somma cumulativa per ottenere il valore `arco`:
- `arco = 0`: Nessun contratto attivo (disoccupazione)
- `arco = 1`: Un contratto attivo (occupazione singola)  
- `arco > 1`: Più contratti attivi (occupazione sovrapposta)

#### Passo 3: Creazione dei Segmenti Temporali

Tra ogni coppia di eventi consecutivi si crea un segmento temporale con:
- **Data di inizio**: Data dell'evento corrente
- **Data di fine**: Data dell'evento successivo
- **Valore arco**: Numero di contratti attivi nel segmento

#### Passo 4: Aggiustamento per la Disoccupazione

Per i periodi di disoccupazione (`arco = 0`), le date vengono aggiustate:
- **Inizio disoccupazione**: Data originale + 1 giorno
- **Fine disoccupazione**: Data originale - 1 giorno

Questo garantisce che non ci siano sovrapposizioni improprie tra periodi di lavoro e disoccupazione.

### Logica delle Date

**Principio fondamentale**: I contratti sono inclusivi di entrambe le date estreme.
- Se un contratto va dal 1° gennaio al 31 gennaio, la persona lavora **sia** il 1° gennaio **sia** il 31 gennaio
- La durata è quindi: 31 - 1 + 1 = 31 giorni

**Gestione dei gap**: Se c'è un vuoto tra due contratti consecutivi:
```
Contratto A: 1 gen - 31 gen
Contratto B: 15 feb - 28 feb
Gap: 1 feb - 14 feb (periodo di disoccupazione)
```

## Esempi Pratici

### Esempio 1: Contratto Singolo

**Dati di input:**
```
id  cf    INIZIO      FINE        prior
1   A001  2023-01-01  2023-01-31  1
```

**Eventi generati:**
```
Data        Evento    Somma cumulativa (arco)
2023-01-01  +1        1
2023-02-01  -1        0
```

**Risultato:**
```
cf    inizio      fine        arco  durata  stato
A001  2023-01-01  2023-01-31  1     31      occ_ft
```

### Esempio 2: Contratti Consecutivi con Gap

**Dati di input:**
```
id  cf    INIZIO      FINE        prior
1   A001  2023-01-01  2023-01-31  1
2   A001  2023-03-01  2023-03-31  0
```

**Eventi generati:**
```
Data        Evento    Somma cumulativa (arco)
2023-01-01  +1        1
2023-02-01  -1        0
2023-03-01  +1        1
2023-04-01  -1        0
```

**Risultato:**
```
cf    inizio      fine        arco  durata  stato
A001  2023-01-01  2023-01-31  1     31      occ_ft
A001  2023-02-01  2023-02-28  0     28      disoccupato
A001  2023-03-01  2023-03-31  1     31      occ_pt
```

### Esempio 3: Contratti Sovrapposti

**Dati di input:**
```
id  cf    INIZIO      FINE        prior
1   A001  2023-01-01  2023-06-30  1
2   A001  2023-04-01  2023-09-30  0
```

**Eventi generati:**
```
Data        Evento    Somma cumulativa (arco)
2023-01-01  +1        1
2023-04-01  +1        2
2023-07-01  -1        1
2023-10-01  -1        0
```

**Risultato:**
```
cf    inizio      fine        arco  durata  stato
A001  2023-01-01  2023-03-31  1     90      occ_ft
A001  2023-04-01  2023-06-30  2     91      over_pt_ft
A001  2023-07-01  2023-09-30  1     92      occ_pt
```

## Struttura dell'Output

Il risultato della funzione `vecshift` è una tabella con queste colonne:

| Colonna | Significato |
|---------|-------------|
| `cf` | Codice fiscale della persona |
| `inizio` | Data di inizio del segmento temporale |
| `fine` | Data di fine del segmento temporale |
| `arco` | Numero di contratti attivi (0=disoccupazione, 1=singolo, >1=sovrapposti) |
| `prior` | Tipo di impiego dominante nel segmento |
| `id` | ID del contratto (0 per periodi di disoccupazione) |
| `durata` | Durata del segmento in giorni |
| `stato` | Classificazione dello stato lavorativo |

### Classificazione degli Stati

La funzione assegna automaticamente uno stato lavorativo a ogni segmento:

- **`disoccupato`**: Nessun contratto attivo (`arco = 0`)
- **`occ_ft`**: Occupazione full-time singola (`arco = 1`, `prior = 1`)
- **`occ_pt`**: Occupazione part-time singola (`arco = 1`, `prior = 0`)
- **`over_pt_ft`**: Sovrapposizione da part-time a full-time
- **`over_ft_pt`**: Sovrapposizione da full-time a part-time  
- **`over_pt_pt`**: Sovrapposizione di contratti part-time
- **`over_ft_ft`**: Sovrapposizione di contratti full-time

## Casi Speciali e Considerazioni

### Contratti di Un Giorno
Se `INIZIO = FINE`, il contratto ha durata di 1 giorno ed è gestito correttamente:
```
INIZIO = FINE = 2023-05-15
Durata = 1 giorno (la persona lavora il 15 maggio)
```

### Contratti Adiacenti
Se due contratti si toccano senza sovrapporsi:
```
Contratto A: FINE = 2023-01-31
Contratto B: INIZIO = 2023-02-01
Risultato: Nessun periodo di disoccupazione tra i contratti
```

### Gestione Errori
La funzione controlla automaticamente:
- Date di fine precedenti alle date di inizio
- Colonne mancanti nei dati
- Formati di data non validi

### Personalizzazione della Classificazione

È possibile personalizzare le regole di classificazione degli stati:

```r
# Regole personalizzate
regole_custom <- create_custom_status_rules(
  unemployment_threshold = 30,  # Soglia per disoccupazione "lunga"
  custom_labels = list(
    unemployed_short = "in_cerca_lavoro",
    full_time = "tempo_pieno",
    part_time = "tempo_parziale"
  )
)

# Applicare le regole personalizzate
risultato <- vecshift(dati, status_rules = regole_custom)
```

## Cosa Significa Questo per Voi

La funzione `vecshift` vi permette di:

1. **Analizzare percorsi lavorativi completi**: Ogni persona diventa una sequenza temporale continua senza vuoti
2. **Identificare pattern occupazionali**: Rilevare facilmente periodi di stabilità, transizioni, sovrapposizioni
3. **Calcolare statistiche precise**: Durata totale di occupazione, frequenza di disoccupazione, intensità lavorativa
4. **Preparare dati per analisi avanzate**: I segmenti temporali sono ideali per modelli statistici e visualizzazioni

**Benefici pratici:**
- **Completezza**: Nessun periodo della vita lavorativa viene perso
- **Precisione temporale**: Date esatte per tutti i cambiamenti di stato
- **Flessibilità**: Classificazioni personalizzabili per diversi contesti di analisi
- **Performance**: Elaborazione efficiente anche su grandi dataset (oltre 1 milione di record al secondo)

La trasformazione da contratti individuali a segmenti temporali continui rappresenta il passaggio fondamentale per qualunque analisi seria dei percorsi lavorativi. La funzione `vecshift` rende questo processo automatico, preciso e affidabile.