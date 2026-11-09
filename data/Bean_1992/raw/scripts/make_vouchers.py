#!/usr/bin/env python3
"""Bean_1992: writes raw/Bean_1992_vouchers.csv from the hand transcription below (read from the page images of the BHL scan).

One line per collection: status | institutions | collector | record number | date | locality | state | country | section | notes.
Several institutions give one row each. Bean 1992 cites no accession numbers except a few in square brackets (catalog column).
"""
import csv
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
L = "Leptospermum "
EX = "specimen examined"

# taxon -> lines
V = {
    L + "pallidum": """
holotype|BRI|A.R. Bean|2949|1991-04-20|Marble Creek mesa, SE of Greenvale, 19 deg 07'S, 145 deg 04'E|Qld|Australia|Type, p.645|
isotype|CANB,K,MEL,NSW|A.R. Bean|2949|1991-04-20|Marble Creek mesa, SE of Greenvale, 19 deg 07'S, 145 deg 04'E|Qld|Australia|Type, p.645|Also Fig. 1 D (fruit).
specimen examined|BRI|Fell|DF796|1988-04|Burke District: Porcupine Gorge|Qld|Australia|Specimens examined, p.645|
specimen examined|BRI|Fell|DF1809|1989-05|North Kennedy District: 20 km east of Greenvale|Qld|Australia|Specimens examined, p.645|
specimen examined|BRI|Fell|DF1810|1989-05|North Kennedy District: 20 km east of Greenvale|Qld|Australia|Specimens examined, p.645|
specimen examined|BRI|Bean|1068|1989-06|North Kennedy District: beside Charters Towers-Greenvale road, 32.3 km from Greenvale|Qld|Australia|Specimens examined, p.645|Also Fig. 1 B, C (flowering twig, flower).
specimen examined|BRI,DNA,MEL,NSW|Bean|2942|1991-04|North Kennedy District: Marble Creek mesa, SE of Greenvale|Qld|Australia|Specimens examined, p.645|
""",
    L + "madidum": """
holotype|BRI|?Persieh||1920|Endeavour River|Qld|Australia|Type of synonym, p.645|Type of the synonym Agonis longifolia C. White & Francis (1920); collection date not stated, year of the protologue given. The collector is printed with a question mark.
isotype|NSW|?Persieh||1920|Endeavour River|Qld|Australia|Type of synonym, p.645|Type of the synonym Agonis longifolia C. White & Francis (1920); collection date not stated, year of the protologue given.
""",
    L + "madidum subsp. madidum": """
specimen examined|BRI|Scarth-Johnson|893A|1979-10|Cook District: bank of Jardine River|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI|Tracey|14495|1974-09|Cook District: north bank of Olive River, near mouth|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI,CANB,DNA,K,L,MO,NSW,PERTH,QRS|Clarkson|3498|1980-10|Cook District: 60 km west of Strathmay on Musgrave to Edward River road|Qld|Australia|Selected specimens, p.646|
specimen examined|JCT|Garnett|ER497|1983-10|Cook District: 10 km SE of Edward River Settlement|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI,NSW|Bean|845|1988-06|Cook District: Archer River, beside main Cape York road|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI|Brass|19699|1948-07|Cook District: Wenlock, Batavia River|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI,NSW|Jobson|733|1989-08|Cook District: Big Bend in Coen River, 2 km N of Coen|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI,CANB,K|Staples|IBS2167|1975-08|Cook District: Hann River|Qld|Australia|Selected specimens, p.646|
specimen examined|BRI|Paijmans|2866|1978-08|Cook District: 15 km east of 'Violet Vale' HS.|Qld|Australia|Selected specimens, p.646|
specimen examined|QRS|Irvine|2175|1981-11|Cook District: Endeavour River, north arm crossing with McIvor R.-Cooktown road|Qld|Australia|Selected specimens, p.646|
""",
    L + "madidum subsp. sativum": """
holotype|BRI|S.T. Blake|17075|1946-09-21|Margaret River|NT|Australia|Type, p.646|
isotype|DNA|S.T. Blake|17075|1946-09-21|Margaret River|NT|Australia|Type, p.646|
specimen examined|CANB,NSW,PERTH|Kenneally|9301|1984-07|Picaninny Creek gorge, 15 km SE of Bungle Bungle outcamp, East Kimberley|WA|Australia|Selected specimens, p.646|
specimen examined|DNA,MEL,PERTH|Menkhorst|748|1989-11|Bream Gorge, Osmund Valley station, East Kimberley|WA|Australia|Selected specimens, p.646|
specimen examined|BRI,DNA|Byrnes|1684|1969-08|Finnis River|NT|Australia|Selected specimens, p.646|
specimen examined|AD,DNA|Dunlop & Wightman|6747|1984-09|Jim Jim Falls|NT|Australia|Selected specimens, p.646|
specimen examined|BRI|Byrnes & Martensz|2822|1972-11|Sawtooth Gorge|NT|Australia|Selected specimens, p.646|
specimen examined|DNA,NSW|Clarke|1469|1987-09|Wooler River, 16 km northwards of Telecom road on Marparu outstation road|NT|Australia|Selected specimens, p.646|
specimen examined|BRI,DNA|Wightman|4960|1990-02|Darwin area|NT|Australia|Selected specimens, p.646|
specimen examined|DNA|Dunlop & Wightman|108|1982-07|Cobourg Peninsula|NT|Australia|Selected specimens, p.646|
""",
    L + "brachyandrum": """
lectotype|NSW|Herman Beckler|||ad ripas fluminis Hastings|NSW|Australia|Type, p.647|Not seen by the author (n.v.). Basionym Kunzea brachyandra F. Muell.
isolectotype|A|Herman Beckler|||ad ripas fluminis Hastings|NSW|Australia|Type, p.647|Seen by the author (!).
specimen examined|BRI,QRS|Bean|3805|1991-11|Cook District: Turtle Rock, 12 km SSE of Laura|Qld|Australia|Additional specimens, p.647|
specimen examined|BRI,MEL,QRS|Bean|3867|1991-12|North Kennedy District: Mt Stuart, 9 km S of Townsville|Qld|Australia|Additional specimens, p.647|
specimen examined|BRI,CANB,K,L,MEL,NSW,PERTH|Bean|3588|1991-08|North Kennedy District: Cockatoo Creek area, Mt Elliot, south of Townsville|Qld|Australia|Additional specimens, p.647|
specimen examined|BRI|Cumming|9294|1989-09|North Kennedy District: Mingela Bluff, about 10 km E of Mingela|Qld|Australia|Additional specimens, p.647|
specimen examined|BRI|Hinson|CU4|1967-06|North Kennedy District: Cape Upstart peninsula|Qld|Australia|Additional specimens, p.647|
specimen examined|BRI|Warrian|CW706|1985-07|North Kennedy District: Stonehaven Bay, Hook Is.|Qld|Australia|Additional specimens, p.647|
""",
    L + "parviflorum": """
holotype|L|Wichmann|52|1903|Archip. Ind. G. Syap.|Irian Jaya|Indonesia|Type, p.647|Type in two pieces on separate sheets, each tagged 52. Undated; Valeton gave the year of collection as 1903. Country inferred from the citation 'New Guinea'.
specimen examined|L|Mangold|2260|1957-02|Irian Jaya. Ransiki|Irian Jaya|Indonesia|Selected specimens, p.647|
specimen examined|A,L|Van Royen|4798|1954-08|Irian Jaya. Boepoel to Merau R., Merauke district|Irian Jaya|Indonesia|Selected specimens, p.647|
specimen examined|L,SING|Leefers|BW3224|1956-08|Irian Jaya. between Boepoel and Tanas|Irian Jaya|Indonesia|Selected specimens, p.647|
specimen examined|A,L,SING|Anta|72|1941-07|Kewa River, Sakoer||Papua New Guinea|Selected specimens, p.647|Listed under Papua New Guinea.
specimen examined|BRI,L,SING|Pleyte|1082|1948-09|Misool, Sorong, near Fakal||Papua New Guinea|Selected specimens, p.647|Listed under Papua New Guinea, although Misool and Sorong are in Indonesia.
specimen examined|A,BRI,L,QRS|Foreman|LAE60470|1974-07|10.5 km west of Arufi, Morehead subdistrict||Papua New Guinea|Selected specimens, p.647|
specimen examined|L|Paijmans|333|1967-08|upper Wanggoe River basin, c. 47 miles [76 km] N of Weam Patrol Post||Papua New Guinea|Selected specimens, p.647|
""",
    L + "purpurascens": """
holotype|NSW|J.R. Clarkson|2196|1978-06-08|Cook District: 12 deg 24'S, 143 deg 07'E, southern end of Temple Bay in upper reaches of an unnamed creek between Glennie and Hunter inlets|Qld|Australia|Type, p.647|Not seen by the author (n.v.).
isotype|BRI|J.R. Clarkson|2196|1978-06-08|Cook District: 12 deg 24'S, 143 deg 07'E, southern end of Temple Bay in upper reaches of an unnamed creek between Glennie and Hunter inlets|Qld|Australia|Type, p.647|Seen by the author (!).
""",
    L + "luehmannii": """
holotype|BRI|F.M. Bailey|4|1884-10|Moreton District: Top of Glass House Mountain|Qld|Australia|Type, p.648|Seen by the author (!).
""",
    L + "speciosum": """
type||A. Cunningham|Herb. no. 38|1824|Moreton District: in Nova Cambria australi, Moreton Bay|Qld|Australia|Type, p.648|Herbarium printed as '?', not seen (n.v.), fide J. Thompson (1989).
""",
    L + "whitei": """
holotype|BRI|W.D. Francis|s.n.||Moreton District: Beerwah|Qld|Australia|Type, p.648|Seen by the author (!). Type of Agonis elliptica C. White & Francis.
""",
    L + "trinervium": """
type|LINN|White||  |[Port Jackson, White] 't. 24, Whites voyage', Sheet 878.11 (top L.H.) herb. Smith|NSW|Australia|Type, p.648|Not seen by the author (n.v.), fide J. Thompson. Basionym Melaleuca trinervia Smith.
lectotype|LINN|Mr White||1795|Port Jackson, Sheet 878.9, R.H. specimens|NSW|Australia|Type of synonym, p.648|Type of the synonym L. attenuatum Smith; seen as a photograph (photo!).
""",
    L + "lamellatum": """
holotype|NSW|N.H. Speck|1843|1963-10-14|Leichhardt District: 21 miles [34 km] SE of Bedourie Homestead|Qld|Australia|Type, p.648|
isotype|BRI,CANB|N.H. Speck|1843|1963-10-14|Leichhardt District: 21 miles [34 km] SE of Bedourie Homestead|Qld|Australia|Type, p.648|
specimen examined|BRI|Cumming|11257|1991-07|Mitchell District: Sandstone Wall, White Mountains NP, 20 deg 27'S, 145 deg 54'E|Qld|Australia|Additional specimens, p.648|
specimen examined|BRI,NSW|Bean|2375|1990-09|South Kennedy District: 4 km N of 'Springvale' homestead, west of Clermont|Qld|Australia|Additional specimens, p.648|
specimen examined|QRS|Godwin|s.n.|1981-05|Leichhardt District: Strike ridge south of Tomahawk Ck, east of Zig Zag Range, 'Peak Vale' holding|Qld|Australia|Additional specimens, p.648|
specimen examined|BRI|Grimshaw|CHR6|1990-06|Maranoa District: beside Redford road, N of Mt Hotspur, near 'Hungry Downs', 26 deg 01'S, 147 deg 30'E|Qld|Australia|Additional specimens, p.648|
specimen examined|BRI|Bean|1166|1989-11|Darling Downs District: 12 km NW of Western Creek Forestry station, via Milmerran|Qld|Australia|Additional specimens, p.648|
""",
    L + "microcarpum": """
lectotype|NSW|E. Cheel||1917-11|Copmanhurst|NSW|Australia|Type, p.648|Catalog number printed as [NSW 154747]; fide J. Thompson (1989).#154747
""",
    L + "brevipes": """
lectotype|MEL|F. Mueller||1853-03-06|Buffalo Creek|Vic|Australia|Type, p.649|Catalog number printed as MEL 1539307; fide J. Thompson (1989).#1539307
isolectotype|K,MEL|F. Mueller||1853-03-06|Buffalo Creek|Vic|Australia|Type, p.649|
specimen examined|AD,BRI,MEL,NSW|Bean|2740|1990-12|Darling Downs District: Bracker State Forest, S of Inglewood|Qld|Australia|Additional specimens, p.649|
specimen examined|BRI,NSW|Bean|2795|1990-12|Darling Downs District: Herries Range, southern end of S.F. 444, south-west of Warwick|Qld|Australia|Additional specimens, p.649|
""",
    L + "neglectum": """
holotype|NSW|E.M. Jackes & B.R. Jackes|s.n.|1982-09-09|North Kennedy District: 17 km west of Paluma|Qld|Australia|Type, p.649|Fide J. Thompson (1989).
specimen examined|BRI,L,MEL,NSW,PERTH,QRS|Clarkson|5916|1985-04|Cook District: Mount Mulligan, c. 40 km NW of Dimbulah|Qld|Australia|Additional specimen, p.649|
""",
    L + "sericatum": """
holotype||T. Mitchell||1846-09-05|Leichhardt District: near the Pyramids [Mt Playfair district]|Qld|Australia|Type, p.649|Herbarium printed as '?', not seen (n.v.).
topotype|BM|T. Mitchell||1846-09-05|Leichhardt District: near the Pyramids [Mt Playfair district]|Qld|Australia|Type, p.649|Seen as a photograph (photo!).
""",
    L + "parvifolium": """
holotype|LINN|Dr White||1795|Port Jackson|NSW|Australia|Type, p.649|Not seen by the author (n.v.), fide J. Thompson (1989).
specimen examined|BRI,MEL,NSW|Bean|2733|1990-12|Darling Downs District: Coolmunda Dam, east of Inglewood|Qld|Australia|Additional specimen, p.649|
""",
    L + "venustum": """
holotype|BRI|A.R. Bean|2112|1990-08-14|Burnett District: 'Melrose' Station, 15 km west of Eidsvold|Qld|Australia|Type, p.649|Also Fig. 1 G (fruit).
isotype|AD,CANB,K,L,MEL,NSW,PERTH,SING|A.R. Bean|2112|1990-08-14|Burnett District: 'Melrose' Station, 15 km west of Eidsvold|Qld|Australia|Type, p.649|
specimen examined|AD,BISH,BRI,CANB,CBG,CONN,DNA,HO,K,L,LAE,MEL,MO,NSW,PERTH,PNH,PR,PRE,QRS,US|Forster|7000|1990-07|Burnett District: 1 km N of Little Morrow Creek crossing, on Eidsvold to Cracow road|Qld|Australia|Specimens examined, p.650|Also Fig. 1 E, F (flowering twig, flower).
specimen examined|BRI|Bancroft|s.n.||Burnett District: Eidsvold|Qld|Australia|Specimens examined, p.650|Catalog number printed as [AQ 041731].#AQ041731
specimen examined|BRI|Bean|283|1985-09|Burnett District: 20.9 km from Eidsvold towards Cracow, N side of road|Qld|Australia|Specimens examined, p.650|
specimen examined|BRI|Bean|1103|1989-07|Burnett District: 20 km W of Eidsvold|Qld|Australia|Specimens examined, p.650|
""",
    L + "semibaccatum": """
lectotype|NSW|E. Cheel|s.n.|1925-05-11|Wallis Island, Tuncurry|NSW|Australia|Type, p.650|Catalog number printed as [NSW 154729]; not seen (n.v.), fide J. Thompson (1989).#154729
specimen examined|BRI|Gibson|TO1860|1989-10|Port Curtis District: Deepwater N.P., 40 km east of Miriam Vale|Qld|Australia|Additional specimen, p.650|
""",
    L + "arachnoides": """
type|BM||||the illustration in Gaertner, Fruct. Sem. Pl. 1: 175, t. 35 (1788), based on a specimen in the Banksian herbarium|||Type, p.650|Fide J. Thompson (1989).
""",
    L + "liversidgei": """
lectotype|NSW|D.W. Munro|s.n.|1905-03|Ballina|NSW|Australia|Type, p.650|Fide J. Thompson (1989).
""",
    L + "oreophilum": """
holotype|NSW|L.A.S. Johnson|s.n.|1951-06-13|Moreton District: Ngungun, Glasshouse Mountains|Qld|Australia|Type, p.650|Catalog number printed as [NSW 154760]; fide J. Thompson (1989).#154760
specimen examined|BRI|Sharpe & Batianoff|3334|1983-07|Moreton District: Mt Coolum, 3 km south of Coolum Beach|Qld|Australia|Additional specimen, p.650|
""",
    L + "polygalifolium": """
holotype|K|Dav. Burton|||juxta Port Jackson [New South Wales]|NSW|Australia|Type, p.651|Herbarium printed as '?K', not seen (n.v.), fide J. Thompson (1989).
holotype|LINN|Mr White|878.8|1795|Port Jackson|NSW|Australia|Type of synonym, p.651|Type of the synonym L. flavescens Smith; seen as a photograph (photo!).
""",
    L + "variabile": """
holotype|NSW|P.R. Sharpe|2438|1978-10-18|Moreton District: Mt Gillies, about 20 km SW of Rathdowney on Mt Lindesay Highway|Qld|Australia|Type, p.651|
isotype|BRI|P.R. Sharpe|2438|1978-10-18|Moreton District: Mt Gillies, about 20 km SW of Rathdowney on Mt Lindesay Highway|Qld|Australia|Type, p.651|
""",
    L + "novae-angliae": """
holotype|NSW|E.F. Constable|2074|1962-03-31|top of Bald Rock Mountain, 15 miles [24 km] north of Tenterfield|NSW|Australia|Type, p.651|Not seen by the author (n.v.), fide J. Thompson (1989).
""",
    L + "minutifolium": """
holotype|BRI|Mrs M.S. Clemens||1944-11|Darling Downs District: base of Mount Norman via Wallangarra|Qld|Australia|Type, p.651|Seen by the author (!).
""",
    L + "juniperinum": """
holotype|LINN|J. White|s.n.|1795|Port Jackson, herb. Smith 878.17|NSW|Australia|Type, p.651|Not seen by the author (n.v.), fide J. Thompson (1989).
""",
    L + "recurvum": """
holotype|K|H. Low|||Kina Balu, abundant, from 7000-8500 feet, whitening the top of the mountain|Borneo|Malaysia|Type, p.652|Not seen by the author (n.v.), fide J. Thompson (1989).
specimen examined|A,SING|Weber|54680|1966-07|Borneo. Kinabalu National Park, Sabah|Sabah|Malaysia|Specimens examined, p.652|Listed under Indonesia.
specimen examined|BRI|Chew & Corner|5877|1964-03|Borneo. East Pinnacles, Mt Kinabalu|Sabah|Malaysia|Specimens examined, p.652|Listed under Indonesia.
specimen examined|A|Clemens|50616|1933-12|Borneo. Gurulau Spur, Mt Kinabalu|Sabah|Malaysia|Specimens examined, p.652|Listed under Indonesia.
specimen examined|A,L|Eyma|1362|1937-07|Sulawesi. top of Kamboeno|Sulawesi|Indonesia|Specimens examined, p.652|
specimen examined|L|Van Balgooy|3323|1979-05|Sulawesi. Mt Roroka Timbu summit|Sulawesi|Indonesia|Specimens examined, p.652|
specimen examined|L|Emya|691|1937-06|Sulawesi. G. Rantemario|Sulawesi|Indonesia|Specimens examined, p.652|Collector printed Emya; probably Eyma, as in the neighbouring citations.
""",
    L + "javanicum": """
holotype|L|||  |Java. in cacumine montis Gede|Java|Indonesia|Type, p.652|Seen by the author (!).
isotype|L|||  |Java. in cacumine montis Gede|Java|Indonesia|Type, p.652|Seen by the author (!).
type||||  |Gunong Bunko, Sumatra|Sumatra|Indonesia|Type of synonym, p.652|Type of the synonym Glaphyria nitida Jack; not seen (n.v.), fide E.D. Merrill (1952).
holotype|L|||  |in declivitatibus altioribus montis Gede|Java|Indonesia|Type of synonym, p.652|Type of the synonym Leptospermum alpestre Blume; not seen (n.v.), fide J. Thompson (1989).
holotype|L|||  |Java. 'javanicae alpinae'|Java|Indonesia|Type of synonym, p.652|Type of the synonym L. floribundum Junghuhn; seen by the author (!).
specimen examined|A|Parker|3112|1930-01|Burma. Myinmolekat, Mergui district||Burma|Selected specimens, p.652|
specimen examined|SING|Holttum|28106|1933-02|Sumatra. Gunong Singgalang|Sumatra|Indonesia|Selected specimens, p.652|
specimen examined|A|Jacobs|8233|1968-05|Sumatra. Mt Tanggamus, Lampung province|Sumatra|Indonesia|Selected specimens, p.652|
specimen examined|SING|Steenis|8569|1937-02|Sumatra. Mt Losir|Sumatra|Indonesia|Selected specimens, p.652|
specimen examined|A|Bremer|1613|1979-09|Malaya. Pahang, Bentong, Genting highlands|Pahang|Malaysia|Selected specimens, p.652|
specimen examined|A|Flenley|4|1969-02|Malaya. Mt Uli Kali, Selangor, Malaya|Selangor|Malaysia|Selected specimens, p.652|
specimen examined|SING|Kloss|12199|1923-06|Malaya. Padang Luas, G. Tahan||Malaysia|Selected specimens, p.652|
specimen examined|A,SING|Whitmore|FRI3288|1967-03|Malaya. Gunong Benom, Pahang|Pahang|Malaysia|Selected specimens, p.652|
specimen examined|BRI,L|Steenis|10613|1938-04|Java. Gunong Gedeh|Java|Indonesia|Selected specimens, p.652|
specimen examined|A,BRI,SING|Chew & Corner|4664|1964-03|Borneo. Mesilau Caves, Sabah|Sabah|Malaysia|Selected specimens, p.652|
specimen examined|A|Clemens|10934|1915-11|Borneo. Marai Parai Spur, Mt Kinabalu|Sabah|Malaysia|Selected specimens, p.652|
specimen examined|A,BRI|Nooteboom|4610|1983-01|Borneo. Bukit Raya||Indonesia|Selected specimens, p.652|Country inferred; printed only under Borneo.
specimen examined|A|Edano|1460|1946-11|Philippines. Mt Apo, 7000ft [2100m], near Sulphur spring||Philippines|Selected specimens, p.652|
specimen examined|A|Edano|22007|1954-04|Philippines. Mt Canlaon, Negros Occidental||Philippines|Selected specimens, p.652|
specimen examined|A,SING|Celestino|4335|1948-03|Philippines. Mt Pulog, Luzon||Philippines|Selected specimens, p.652|
specimen examined|L|Smith|681|1981-02|Sulawesi. Gunong Rantemario|Sulawesi|Indonesia|Selected specimens, p.652|
specimen examined|A,L|Eyma|604|1937-06|Sulawesi. Pokapin-djang|Sulawesi|Indonesia|Selected specimens, p.652|Also named under Affinities, p.653: obovate leaves about 14 mm long.
specimen examined|L|Whitten|1985||Sulawesi|Sulawesi|Indonesia|Affinities, p.653|Named only under Affinities: obovate leaves about 14 mm long.
""",
    L + "wooroonooran": """
holotype|BRI|F.M. Bailey||1889-06-22|Cook District: South Peak, Bellenden-Ker|Qld|Australia|Type, p.653|Seen by the author (!).
specimen examined|QRS|Godwin|s.n.|1981-10|Cook District: Devils Thumb|Qld|Australia|Additional specimen, p.653|
""",
    L + "amboinense": """
holotype|L|collector unknown|||Moluccas. Amboina|Moluccas|Indonesia|Type, p.653|Seen by the author (!).
type||A. Schadenberg||1882-02|Mt Apo||Philippines|Type of synonym, p.653|Type of the synonym L. annae Stein; not seen (n.v.).
type|||  |  |Malaya. Kedah Peak||Malaysia|Type of synonym, p.653|Type of the synonym L. flavescens var. angustifolia Ridley; not seen (n.v.).
holotype|NSW|C. Debenham|s.n.|1965-09|cultivated Castle Hill [Sydney] from seed ex Herberton dist.|NSW|Australia|Type of synonym, p.653|Type of the synonym L. petersonii subsp. lanceolatum J. Thompson; seen by the author (!).
isotype|BRI|C. Debenham|s.n.|1965-09|cultivated Castle Hill [Sydney] from seed ex Herberton dist.|NSW|Australia|Type of synonym, p.653|Type of the synonym L. petersonii subsp. lanceolatum J. Thompson; seen by the author (!).
specimen examined|SING|Symington|CF25120|1930-12|Sumatra. Brastagi|Sumatra|Indonesia|Selected specimens, p.653|
specimen examined|A,SING|Whitmore|FRI12354|1969-07|Malaya. Gunong Ledang (Mt Ophir)||Malaysia|Selected specimens, p.653|
specimen examined|BRI|Shukor|AS1|1970-12|Malaya. Gunong Panti, Johore|Johore|Malaysia|Selected specimens, p.653|
specimen examined|SING|Burkill|HMB3324|1964-01|Malaya. Gunong Jerai (Kedah Peak)|Kedah|Malaysia|Selected specimens, p.653|
specimen examined|A,L|Kostermans & Wirawan|594|1965-04|Lesser Sunda Islands. Manau near Ruteng, W. Flores|Flores|Indonesia|Selected specimens, p.653|
specimen examined|L|Verheijen|3359|1973-03|Lesser Sunda Islands. summit of Gunong Ranaka, Flores|Flores|Indonesia|Selected specimens, p.653|
specimen examined|A|Mjoberg|238||Borneo. Mt Santubong, Sarawak|Sarawak|Malaysia|Selected specimens, p.653|Date printed as s.d.
specimen examined|SING|Wong|WKM820|1989-01|Borneo. Summit of Mt Retak, Brunei||Brunei|Selected specimens, p.653|
specimen examined|BRI,NSW|Ramos & Pascasio|34493|1919-04|Philippines. Surigao Province||Philippines|Selected specimens, p.653|
specimen examined|SING|Edano|1371|1946-11|Philippines. Baklayan, Mt Apo, Mindanao||Philippines|Selected specimens, p.653|
specimen examined|A,SING|Ramos & Convocar|84002|1931-05|Philippines. Dinagat Island, Mindanao||Philippines|Selected specimens, p.653|
specimen examined|L|Meijer|11042|1976-07|Sulawesi. South slope of Mt Bonthain|Sulawesi|Indonesia|Selected specimens, p.653|
specimen examined|A|Curran|321|1940-05|Moluccas. Manipa Island|Moluccas|Indonesia|Selected specimens, p.653|
specimen examined|A|Buwalda|6207|1938-10|Moluccas. Kp. Waai, Gunong Salahoetoe, Ambon|Moluccas|Indonesia|Selected specimens, p.653|
specimen examined|SING|Eyma|2980|1938-02|Moluccas. Kaibobo-Oernitoe, W. Ceram|Moluccas|Indonesia|Selected specimens, p.653|
specimen examined|BRI|Smith|11147|1960-09|Cook District: Hoop Pine area, near McIvor|Qld|Australia|Selected specimens, p.653|
specimen examined|BRI|Flecker|14258|1952-07|Cook District: Big Tableland, near Cooktown|Qld|Australia|Selected specimens, p.653|The Note, p.654, calls the Big Tableland form unusual: 2--4 flowers per inflorescence.
specimen examined|BRI|Bean|3205|1991-05|North Kennedy District: Frederick Peak, 25 km SW of Townsville|Qld|Australia|Selected specimens, p.653|
specimen examined|BRI,K,L,MEL,NSW,SING|Bean|3364|1991-06|North Kennedy District: Roma Peak, 40 km S of Bowen|Qld|Australia|Selected specimens, p.653|
""",
    L + "petersonii": """
holotype|BRI|W.J. Peterson||1905-01|Wilsons Peak|Qld|Australia|Type, p.654|Seen by the author (!).
isotype|NSW|W.J. Peterson||1905-01|Wilsons Peak|Qld|Australia|Type, p.654|
specimen examined|BRI,NSW|Bean|2820|1990-12|Wide Bay District: Mt Tinbeerwah, 6 km west of Tewantin|Qld|Australia|Additional specimens, p.654|
specimen examined|BRI|Smith|742|1940-01|Darling Downs District: Red Rock Gorge, near Ballandean|Qld|Australia|Additional specimens, p.654|
specimen examined||Thompson|4156||||Australia|Fig. 1 caption, p.656|Fig. 1 A (twig with stem flanges); named only in the caption.
""",
    L + "gregarium": """
holotype|NSW|J. Thompson|4196|1981-07-23|10 km from Ebor on Guyra road|NSW|Australia|Type, p.654|Not seen by the author (n.v.), fide J. Thompson (1989).
specimen examined|BRI|Grimshaw|s.n.|1977-08|Darling Downs District: Girraween National Park, Portion 125, between Bald Rock and South Bald Rock|Qld|Australia|Additional specimen, p.654|Catalog number printed as [AQ 438016].#AQ438016
""",
}

COLS = ["taxon_name", "specimen_status", "institution_code", "catalog_number", "collector", "record_number", "collection_date", "locality",
        "state", "country", "source_section", "notes"]
out = []
for taxon, block in V.items():
    for line in block.strip().splitlines():
        p = line.split("|")
        if len(p) != 10:
            raise SystemExit(f"{taxon}: {len(p)} fields: {line}")
        status, insts, coll, rec, date, loc, state, country, sec, notes = [x.strip() for x in p]
        catalog = ""
        if "#" in notes:
            notes, catalog = notes.split("#")
        for inst in insts.split(",") if insts else [""]:
            out.append(dict(taxon_name=taxon, specimen_status=status, institution_code=inst, catalog_number=catalog, collector=coll,
                            record_number=rec, collection_date=date, locality=loc.replace("deg ", "°"), state=state, country=country,
                            source_section=sec, notes=notes))
with open(ROOT / "data" / "Bean_1992" / "raw" / "Bean_1992_vouchers.csv", "w", newline="") as f:
    w = csv.DictWriter(f, COLS)
    w.writeheader()
    w.writerows(out)
print(len(out), "voucher rows;", len(V), "taxa")
