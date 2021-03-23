read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>% 
  rename(trait_name = Character) %>%
  distinct(Character_state,.keep_all = TRUE) %>% write_csv("data/eFLOWER_2021/raw/traits_values1.csv")

#taxon match
read_csv("config/taxon_list.csv") %>%
  mutate(taxa_to_keep = 1) %>%
  rename(Taxon = taxon_name) %>%
  select(Taxon","taxa_to_keep) -> taxa_in_Austraits

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Character_state)) %>% 
  mutate(trait_value = Character_state) -> character_values

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Value)) %>% 
  subset(is.na(Max)) %>%
  mutate(trait_value = Value) %>%
  mutate(trait_value = as.character(trait_value)) -> mean_values

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Min)) %>% 
  mutate(trait_value = Min) %>%
  mutate(Character = paste0(Character,"_min")) %>%
  mutate(trait_value = as.character(trait_value)) -> min_values

read_csv("data/eFLOWER_2021/raw/data_raw.csv") %>%
  subset(!is.na(Max)) %>% 
  mutate(trait_value = Max) %>%
  mutate(Character = paste0(Character,"_max")) %>%
  mutate(trait_value = as.character(trait_value)) -> max_values

character_values %>%
  bind_rows(mean_values) %>%
  bind_rows(min_values) %>%
  bind_rows(max_values) %>% 
  rename(Taxon = Species) %>%
  left_join(taxa_in_Austraits) %>%
  group_by(NDat,Taxon,Character,trait_value) %>%
  mutate_at(vars(trait_value),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  subset(!(Species %in% c("Ascarina lucida","Gilia capitata","Hortonia floribunda","Mazus pumilus","Abatia
                         parviflora","Abelia triflora","Acalypha californica","Acanthochlamys bracteata","Acanthus
                         montanus","Acharia tragodes","Acicarpha tribuloides","Acorus gramineus","Acridocarpus
                         natalitius","Adoxa moschatellina","Aesculus pavia","Aextoxicon punctatum","Afrolicania
                         elaeosperma","Afrostyrax lepidophyllus","Agave ghiesbreghtii","Alangium chinense",
                         "Albizia julibrissin","Allium fistulosum","Alluaudia procera","Alnus alnobetula","Alophia
                         drummondii","Alseuosmia macrophylla","Altingia excelsa","Amborella trichopoda","Ancistrocladus
                         korupensis","Androsace spinulifera","Androstachys johnsonii","Anemopsis californica",
                         "Aneulophus africanus","Angelica sylvestris","Anisophyllea fallax","Anisoptera marginata",
                         "Aphloia theiformis","Aporosa frutescens","Aralidium pinnatifidum","Archytaea triflora",
                         "Arctopus echinatus","Argophyllum laxum","Aristea glauca","Aristolochia macrophylla",
                         "Asarum canadense","Asimina triloba","Asteropeia micraster","Astragalus membranaceus",
                         "Atuna racemosa","Aucuba japonica","Balanops vieillardii","Barbeuia madagascariensis",
                         "Barbeya oleoides","Barnadesia caryophylla","Batis maritima","Beaucarnea recurvata",
                         "Begonia sanguinea","Berberidopsis corallina","Bergia texana","Berzelia albiflora",
                         "Berzelia lanuginosa","Bhesa paniculata","Biebersteinia orphanidis","Bomarea edulis",
                         "Bonnetia sessilis","Boopis graminea","Brexia madagascariensis","Brunellia acutangula",
                         "Bulbine succulenta","Burmannia biflora","Bursera fagaroides","Buxus sempervirens",
                         "Calceolaria integrifolia","Callicarpa dichotoma","Calycanthus floridus","Campanula
                         elatines","Campanula trachelium","Camptotheca acuminata","Canella winterana","Cardiopteris
                         quinqueloba","Carludovica palmata","Caryocar glabrum","Casearia sylvestris","Cassipourea
                         lanceolata","Catalpa speciosa","Caulophyllum thalictroides","Ceanothus sanguineus",
                         "Celastrus scandens","Celtis yunnanensis","Centroplacus glaucinus","Cercidiphyllum
                         japonicum","Cercis canadensis","Cespedesia spathulata","Chamaedorea seifrizii","Chimonanthus
                         praecox","Chloranthus japonicus","Choristylis rhamnoides","Chrysobalanus icaco","Chrysolepis
                         sempervirens","Cinnamodendron ekmanii","Circaeaster agrestis","Clarkia xantiana",
                         "Clavija eggersiana","Claytonia virginica","Clethra alnifolia","Clidemia petiolaris",
                         "Clusia gundlachii","Clutia pulchella","Coccinia sessilifolia","Cocculus orbiculatus",
                         "Codiaeum peltatum","Colchicum speciosum","Columellia oblonga","Conceveiba martiana",
                         "Connarus championii","Corbichonia decumbens","Coriaria ruscifolia","Corokia cotoneaster",
                         "Corylopsis pauciflora","Coula edulis","Couroupita guianensis","Crassula rupestris",
                         "Cratoxylum cochinchinense","Crinodendron hookerianum","Croizatia brevipetiolata",
                         "Croomia pauciflora","Crossosoma bigelovii","Crossostylis grandiflora","Croton alabamensis",
                         "Crypteronia paniculata","Ctenolophon englerianus","Curtisia dentata","Cuscuta cuspidata",
                         "Cyphia elata","Cypripedium calceolus","Cyrilla racemiflora","Dalechampia spathulata",
                         "Danthonia spicata","Dapania racemosa","Daphniphyllum macropodum","Datisca cannabina",
                         "Decaisnea fargesii","Degeneria vitiensis","Delosperma echinatum","Dendrosicyos socotrana",
                         "Desfontainia spinosa","Dialypetalum floribundum","Dicella nucifera","Dicentra eximia",
                         "Dichapetalum rugosum","Didymeles perrieri","Diervilla sessilifolia","Dillenia retusa",
                         "Diospyros lotus","Dipelta yunnanensis","Dipentodon sinicus","Dirachma socotrana",
                         "Disanthus cercidifolius","Donatia fascicularis","Dovyalis rhamnoides","Drimys winteri",
                         "Drosophyllum lusitanicum","Drypetes madagascariensis","Dudleya viscida","Durandea
                         pentagyna","Durio zibethinus","Elaeodendron orientale","Eliea articulata","Endospermum
                         moluccanum","Enkianthus campanulatus","Eriocaulon aquaticum","Erythrospermum phytolaccoides",
                         "Erythroxylum confusum","Euclea crispa","Eucnide bartonioides","Eucommia ulmoides",
                         "Euonymus americanus","Euphorbia epithymoides","Euphronia guianensis","Euptelea polyandra",
                         "Eurya japonica","Exacum affine","Exbucklandia populnea","Fagus grandifolia","Fendlera
                         rupicola","Floerkea proserpinacoides","Forgesia racemosa","Forstera bidwillii","Fouquieria
                         splendens","Fuchsia procumbens","Gaiadendron punctatum","Galax urceolata","Galearia
                         filiformis","Garcinia subelliptica","Geissoloma marginatum","Gentiana saponaria",
                         "Gerbera jamesonii","Gerrardina foliosa","Gisekia africana","Gladiolus buckerveldii",
                         "Glaucidium palmatum","Gomortega keule","Gomphandra javanica","Gonocaryum litorale",
                         "Goupia glabra","Greyia radlkoferi","Grubbia tomentosa","Guaiacum sanctum","Guamatela
                         tuerckheimii","Gunnera hamiltonii","Halesia carolina","Halleria lucida","Halophytum
                         ameghinoi","Hamamelis virginiana","Hedycarya arborea","Hedyosmum arborescens","Heisteria
                         parvifolia","Helianthemum nummularium","Helwingia japonica","Heptacodium miconioides",
                         "Heteromorpha arborescens","Heteropyxis natalensis","Heuchera micrantha","Heywoodia
                         lucens","Hirtella bicornis","Houttuynia cordata","Hua gabonii","Hugonia platysepala",
                         "Humiria balsamifera","Hura crepitans","Hybanthus concolor","Hydnocarpus heterophylla",
                         "Hydrastis canadensis","Hydrolea ovata","Hydrophyllum capitatum","Hydrostachys multifida",
                         "Hymenanthera alpina","Hypecoum imberbe","Hypertelis spergulacea","Hypoxis hemerocallidea",
                         "Icacina mannii","Idesia polycarpa","Ilex cornuta","Illicium floridanum","Impatiens
                         repens","Indigofera heterantha","Iris missouriensis","Irvingia malayana","Itea virginica",
                         "Ixerba brexioides","Ixiolirion tataricum","Japonolirion osense","Joinvillea plicata",
                         "Juglans mandshurica","Junellia succulentifolia","Justicia americana","Kadsura japonica",
                         "Kiggelaria africana","Kingdonia uniflora","Klainedoxa gabonensis","Koeberlinia spinosa",
                         "Kolkwitzia amabilis","Krameria ixine","Lachnostylis bilocularis","Lacistema aggregatum",
                         "Lactoris fernandeziana","Lampranthus blandus","Lapageria rosea","Lardizabala biternata",
                         "Larrea tridentata","Lasiocroton bahamensis","Leea guineensis","Leonia glycycarpa",
                         "Lepidobotrys staudtii","Lilium superbum","Limeum africanum","Limonium arborescens",
                         "Linnaea borealis","Linum perenne","Liquidambar styraciflua","Liriodendron chinense",
                         "Lissocarpa benthamii","Lophopyxis maingayi","Lozania pittieri","Luculia gratissima",
                         "Lunania parviflora","Luxemburgia octandra","Lysimachia tenella","Maesa tenera","Magnolia
                         tripetala","Mahonia bealei","Malesherbia linearifolia","Malpighia emarginata","Mammea
                         americana","Manilkara zapota","Maranta cristata","Marathrum rubrum","Marcgravia trinitatis",
                         "Mauloutchia chapelieri","Medusagyne oppositifolia","Melanophylla alnifolia","Meliosma
                         veitchiorum","Menispermum canadense","Mentzelia lindleyi","Menyanthes trifoliata",
                         "Metanarthecium luteoviride","Microdesmis puberula","Mimosa polycarpa","Minquartia
                         guianensis","Misodendrum linearifolium","Mitchella repens","Montinia caryophyllacea",
                         "Morina longifolia","Morus indica","Moschopsis rosulata","Moultonianthus leembruggianus",
                         "Mouriri cyphocarpa","Myodocarpus fraxinifolius","Myrica cerifera","Myrica gale","Myriophyllum
                         sibiricum","Myristica fragrans","Myrothamnus flabellifolia","Nardostachys chinensis",
                         "Nelumbo lutea","Neoscortechinia kingii","Nepenthes alata","Nephrophyllidium crista-galli",
                         "Nitraria retusa","Nolana humifusa","Nothofagus antarctica","Nothoscordum bivalve",
                         "Nuphar advena","Nymphoides peltata","Nyssa ogeche","Ochanostachys amentacea","Ochna
                         multiflora","Ochroma pyramidale","Ochthocosmus longipedicellatus","Oenothera parviflora",
                         "Olinia ventosa","Omphalea diandra","Oncotheca balansae","Orontium aquaticum","Osyris
                         lanceolata","Oxalis dillenii","Pachysandra procumbens","Paeonia californica","Panax
                         quinquefolius","Panda oleosa","Pangium edule","Paracryphia alticola","Paradrypetes
                         subintegrifolia","Parnassia palustris","Paropsia madagascariensis","Passiflora biflora",
                         "Patrinia triloba","Paxistima canbyi","Pedicularis foliosa","Pelargonium cotyledonis",
                         "Peltanthera floribunda","Pentadiplandra brazzeana","Pentaphragma ellipticum","Penthorum
                         sedoides","Peperomia caliginigaudens","Pera bicolor","Peridiscus lucidus","Perrottetia
                         ovata","Petalonyx nitidus","Petrosavia sakuraii","Peumus boldus","Phalaenopsis aphrodite",
                         "Phaulothamnus spinescens","Phelline billardierei","Phenakospermum guyannense","Philadelphus
                         lewisii","Philesia magellanica","Phlox longifolia","Photinia x fraseri","Phryma leptostachya",
                         "Phyllanthus flexuosus","Phyllonoma laticuspis","Physena madagascariensis","Picramnia
                         polyantha","Pilea cadierei","Pinguicula moranensis","Pittosporum tobira","Plagiopteron
                         suaveolens","Platanus occidentalis","Platyspermation crassifolium","Pleea tenuifolia",
                         "Plocosperma buxifolium","Podocalyx loranthoides","Podophyllum peltatum","Podostemum
                         ceratophyllum","Pogonophora schomburgkiana","Polemonium reptans","Poliothyrsis sinensis",
                         "Polygala pauciflora","Polyscias guilfoylei","Populus tremuloides","Potamogeton berchtoldii",
                         "Primula sieboldii","Prockia crucis","Pseudonemacladus oppositifolius","Pseudopanax
                         arboreus","Pterocephalodes hookeri","Pterostemon rotundifolius","Puya raimondii",
                         "Qualea grandiflora","Quercus rubra","Quiina pteridophylla","Quillaja saponaria","Ravenala
                         madagascariensis","Rhabdodendron amazonicum","Rhamnus cathartica","Rhododendron hippophaeoides",
                         "Rhodohypoxis milloides","Rhodoleia championii","Rhus copallinum","Rhynchoglossum
                         notonianum","Rhynchospora latifolia","Ribes aureum","Rinorea pubiflora","Roridula
                         gorgonias","Roupala montana","Roussea simplex","Ruptiliocarpon caracolito","Sabia
                         swinhoei","Sacoglottis amazonica","Salix reticulata","Sambucus racemosa","Sanicula
                         gregaria","Sarcandra chloranthoides","Sarcobatus vermiculatus","Sargentodoxa cuneata",
                         "Sarracenia purpurea","Saruma henryi","Sassafras albidum","Saururus cernuus","Sauvagesia
                         erecta","Saxifraga cernua","Scabiosa columbaria","Schisandra chinensis","Schoepfia
                         schreberi","Scrophularia californica","Scyphostegia borneensis","Simmondsia chinensis",
                         "Sinadoxa corydalifolia","Siparuna decipiens","Sloanea latifolia","Smilax glauca",
                         "Soyauxia talbotii","Sparganium eurycarpum","Spathiostemon javensis","Spathiphyllum
                         wallisii","Spigelia marilandica","Spinacia oleracea","Spiraea betulifolia","Stachyurus
                         praecox","Stackhousia minima","Staphylea trifolia","Stegnosperma halimifolium","Stegolepis
                         ligulata","Sterculia apetala","Strasburgeria robusta","Strelitzia reginae","Styrax
                         officinalis","Sullivantia oregana","Suregada boiviniana","Swietenia macrophylla",
                         "Symphonia tanalensis","Symplocos zizyphoides","Syringa vulgaris","Tacca chantrieri",
                         "Takhtajania perrieri","Tamarix chinensis","Tapiscia sinensis","Tapura guianensis",
                         "Tecophilaea cyanocrocus","Ternstroemia stahlii","Tetracentron sinense","Tetracera
                         asiatica","Tetracoccus dioicus","Tetradoxa omeiensis","Tetramerista crassifolia",
                         "Tetraplasandra hawaiensis","Tetrapterys tinifolia","Tetrorchidium gabonense","Thomandersia
                         laurifolia","Thottea tomentosa","Thryallis latifolia","Thymelaea hirsuta","Tinospora
                         sinensis","Titanotrichum oldhamii","Tofieldia calyculata","Torricellia tiliifolia",
                         "Touroulia guianensis","Tovaria pendula","Trichilia emetica","Triglochin maritima",
                         "Trigonia nivea","Trigonostemon verrucosus","Trillium erectum","Triosteum perfoliatum",
                         "Triphyophyllum peltatum","Triplostegia glandulifera","Tripterygium wilfordii","Trochodendron
                         aralioides","Tropaeolum tricolor","Utricularia alpina","Vaccinium uliginosum","Vahlia
                         capensis","Valdivia gayana","Valeriana officinalis","Vantanea guianensis","Viburnum
                         acerifolium","Viola pubescens","Vismia baccifera","Vitekorchis excavata","Vitis aestivalis",
                         "Viviania marifolia","Vochysia guatemalensis","Vriesea psittacina","Weigela hortensis",
                         "Xanthorhiza simplicissima","Xerophyta elegans","Xerophyta retinervis","Xerosicyos
                         danguyi","Xyris jupicai","Yucca filamentosa","Zabelia tyaihyonii","Zelkova serrata",
                         "Zingiber gramineum"))) %>%
  write_csv("data/eFLOWER_2021/data.csv")

#excluded data:
subset(austraits$excluded_data,dataset_id==current_study & error!="Missing value") %>%
  write_csv("data/eFLOWER_2021/raw/excluded_data.csv")

subset(austraits$traits,dataset_id==current_study) %>%
  write_csv("data/eFLOWER_2021/raw/data_in_Austraits.csv")

austraits$traits %>%
  subset(dataset_id == "eFLOWER_2021") -> eFLOWER

eFLOWER %>%
  distinct(taxon_name) %>%
  as.data.frame() %>%
  anti_join(taxa_in_Austraits) %>%
  write_csv("data/eFLOWER_2021/raw/eFLOWER_taxa_not_in_AusTraits.csv")

taxa_in_Austraits %>%
  rename(taxon_name = Taxon) -> taxa_in_Austraits


