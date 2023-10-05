###### Info.Readme ######

Nella cartella "Progetto" è possibile trovare, oltre a questo stesso file testuale di informazioni, 
un file "Relazione_Progetto.Rmd" nel quale si trova il sorgette per ottenere il pdf caricato su moodle e
tre cartelle :

- Environment 
	[ Nella seguente cartella è possibile trovare tutti gli environment utilizzati nel file Rmd, tali 
	  environment sono stati creati al fine di attuare un'analisi in compartimenti, cosi da facilitare
	  la creazione del pdf tramite MarkDown ]

- Codici :
	# 0_Intro_to_problem.R
		[ Script nel quale viene mostrato il problema in esame, nello specifico si visualizza 
		  un immagine a dimensione reale e una a dimensione ridotta come utilizzata nell'analisi,
		  si possono anche visualizzare le prime righe dei dataset accelerometer_split1.csv,
		  gyroscope_split1.csv e record1.csv che si riferiscono ai dati sensoriali raccolti
		  nella prima giornata di osservazioni]

	# 1_Processing_Images.R
		[ Script nel quale vengono vettorizzati i pixel relativi al primo canale ovvero quello 
		  contenente le scale di grigi ]

	# 2_Processing_Accelerometer.R
		[ Script nel quale si calcola per ogni identificativo di secondo la deviazione standard
		  dei valori assunti sull'asse z dall'accelerometro ]

	# 3_Processing_Gyroscope.R
		[ Script nel quale si calcola per ogni identificativo di secondo la deviazione standard
		  dei valori assunti sull'asse y dal giroscopio ]

	# 4_Definition_Model_Data.R
		[ Script nel quale si definiscono le etichette da utilizzare tramite FLSA e K-means,
		  viene anche fatta un'analisi esplorativa per valutare l'andamento delle variabili 
		  numeriche, ovvero la velocit e le due sd relative a z accelerometro e y giroscopio ]

	# 5_Alternative_Dataset.R
		[ Script nel quale vengono definiti due dataset alternativi da utilizzare per la modellazione,
		  uno risultante di un FLSA 1D sui pixel vettorizzati per ogni immagine, e uno che permette
		  di zoomare le immagini direttamente sul terreno che è di interesse classificare ]

	# 6_Fused_labels_k2_Models.R
		[ Script nel quale si stimano i modelli relativi alle etichette trovate tramite procedura FLSA ]

	# 7_Kmeans_Labels_k2_Models.R
		[ Script nel quale si stimano i modelli relativi alle etichette trovate tramite procedura K-means ]

	# 8_Kaggle_Labels_k2_Models.R
		[ Script nel quale si stimano i modelli relativi alle etichette fornite dai ricercatori ]

	# 9_Best_Models_Alternative_Dataset_FLSA.R
		[ Script nel quale si stimano i migliori modelli per ogni etichetta sul set di dati relativo ai pixel
		  su i quali è stato applicato FLSA 1D ] 

	# 10_Best_Models_Alternative_Dataset_ImageReduce.R
		[ Script nel quale si stimano i migliori modelli per ogni etichetta sul set di dati relativo ai pixel
		  riferiti al terreno zoomato ] 



- Dataset : 
	[ cartella nella quale sono presenti sia dataset scaricati dalla piattaforma Kaggle tramite il 
	  link : " https://www.kaggle.com/datasets/magnumresearchgroup/offroad-terrain-dataset-for-autonomous-vehicles?select=ImageLabels ",
	  e dataset processati da quelli di partenza ]

	#### Dataset originali ####

	# accelerometer_calibrated_split1.csv
	# accelerometer_calibrated_split2.csv
	# accelerometer_calibrated_split3.csv
	# accelerometer_calibrated_split4.csv
	# accelerometer_calibrated_split5.csv
		[ Dataset che rappresentano i valori misurati sull'accelerometro per ogni rispettivo spliti-esimo, 
		  dove i rappresenta il giorno di rilevazione ]
	
	# gyroscope_calibrated_split1.csv
	# gyroscope_calibrated_split2.csv
	# gyroscope_calibrated_split3.csv
	# gyroscope_calibrated_split4.csv
	# gyroscope_calibrated_split5.csv
		[ Dataset che rappresentano i valori misurati sul giroscopio per ogni rispettivo spliti-esimo, 
		  dove i rappresenta il giorno di rilevazione ]

	# record1.csv
	# record2.csv
	# record3.csv
	# record4.csv
	# record5.csv
		[ Dataset che rappresentano informazioni aggiuntive sulle rilevazioni effettuate (es : velocita, lat, long),
              per ogni rispettivo recordi-esimo, dove i rappresenta il giorno di rilevazione ]

	# tsm_1_labels.csv
		[ Dataset contenente le etichette ottenute dai ricercatori di Kaggle ]

	# Images [ Cartella ] :
					# Images [ Cartella ] :
									# 2020-10-02
									# 2020-09-29
									# 2020-09-24
									# 2020-09-23
									# 2020-07-28
		[ All'interno delle cartelle Images -> Images è possibile trovare altre
		  5 cartelle contenenti i frame d'interesse divisi per giorni di osservazione ]

	#### Dataset processati ####
	
	# Z_Acc.csv
		[ Dataset contenente tutte le misurazioni dell'asse z relative all'accelerometro ]

	# Y_Sd_Gyro.csv
		[ Dataset contenente le standard deviation delle rilevazioni sull'asse y relative
		  al giroscopio ]

	# Y_Sd_Acc.csv
		[ Dataset contenente le standard deviation delle rilevazioni sull'asse z relative 
		  all'accelerometro ]

	# Pixel_piccoli.csv
		[ Dataset contenente i pixel relative alle immagini zoomate sulla strada ]

	# Matrice_fused.csv
		[ Dataset contenente i pixel ai quali è stato applicato Fused-Lasso-Signal-Approximator ]

	# Data_k2_k3_models.csv
		[ Dataset contenete sia le varie etichette sia i pixel vettorizzati presi dalle immagini di partenza ]


