/**
* Name: epidemie
* Based on the internal empty template. 
* Author: user
* Tags: 
*/


model epidemie

/* Insert your model definition here */

global{
	
	

	/*
	 * import GIS shapefile
	 */
	file rues_shape_file <- file("../includes/final_roads.shp");
	file places_shape_file <- file("../includes/final_buldings.shp");
	
	file ville_shape_file <- file("../includes/final_ville.shp");
	geometry shape <- envelope( envelope(places_shape_file)+envelope(ville_shape_file));
	
	graph chemins;
	
	
	/** definition des variables globales */
	
	//attribut global
	int jour_courant<-0;
	int nombre_tour<-0;
	int nombre_mort<-0;
	int nombre_guerrison<-0;
	int seuil_quarantaine<-4;
	int nombre_medecin_urgence<-2;
	rgb quarantaine_color<-#red;
	int nb_personne <- 3000 parameter: 'Nombre personne normal' category: 'Personnes normales' min:1000 max:100000;
	int nb_infecte <- 700 parameter: "Nombre personne infecte" category: 'Personnes infectes' min:0 max:100000;
	int nb_malade <- 100 parameter: "Nombre personne malade" category: 'Personnes infectes' min:0 max:100000;
	int nb_doc <- 10 parameter: "Nombre de docteurs par centre de sante" category:' docteurs' min:0 max:10;
	
	init {
		
       
		create ville from: ville_shape_file;
		create rue from:rues_shape_file;
		chemins <-as_edge_graph(rue);
		//places distributions
		list<string> types_places<-["maison","maison","maison","maison","travail","travail","ecole","ecole","hospital","loisir"];
		int compteur<-0;
		create place from: places_shape_file with: [type_place::string(read('building'))] {
			
			if type_place="" or type_place=nil
			{
				type_place<-types_places[compteur];
				compteur<-compteur+1;
			}
			if compteur=10
            {
			  compteur<-0;
			}
			if type_place="maison"
			{
			    //write "maison";
				type_place<-"maison";
				color<-#indigo;
			}
			if  type_place="travail"
			{
				type_place<-"travail";
				color<-#gray;
			}
			if  type_place="ecole"
			{
			    type_place<-"ecole";
				color<-#green;
			}

			if type_place="hospital"
			{
				color<-#darkslategray;
			}
			if type_place="loisir"
			{
				color<-#olive;
			}
			initial_color<-color;
		}
		
		//creattion des autres agents
		list<place> hospitals<- place where (each.type_place="hospital");
		list<place> maisons <- place where (each.type_place="maison");
		list<place> ecoles<- place where (each.type_place="ecole");
		list<place> loisirs <- place  where (each.type_place="loisir");
		list<place> travails <- place where (each.type_place="travail");
		
		
		create personne number: nb_personne {
		 	lieu_maison <- one_of(maisons);
		 	duree_rester_lieu<-duree_maison;
		 	target<-any_location_in(lieu_maison);
		 	location <- target;
		 	place_courante<-0;
		 	if(age<=20)
		 	{
		 		lieu_travail<- one_of(ecoles);
		 	}
		 	if(age>20 and age<=70)
		 	{
		 		lieu_travail<-one_of(travails);
		 	}
		 		
		 }
		 
		 create personne number: nb_infecte{
		 	est_infecte<-true;
		 	color<-#sienna;
		 	lieu_maison <- one_of(maisons);
		 	duree_rester_lieu<-duree_maison;
		 	target<-any_location_in(lieu_maison);
		 	location <- target;
		 	place_courante<-0;
		 	if(age<=20)
		 	{
		 		lieu_travail<- one_of(ecoles);
		 	}
		 	if(age>20 and age<=70)
		 	{
		 		lieu_travail<-one_of(travails);
		 	}
		 		
		 }
		 
		 create personne number: nb_malade{
		 	est_infecte<-true;
		 	color<-#red;
		 	lieu_maison <- one_of(maisons);
		 	duree_rester_lieu<-duree_maison;
		 	target<-any_location_in(lieu_maison);
		 	location <- target;
		 	place_courante<-0;
		 	if(age<=20)
		 	{
		 		lieu_travail<- one_of(ecoles);
		 	}
		 	if(age>20 and age<=70)
		 	{
		 		lieu_travail<-one_of(travails);
		 	}
		 		
		 }
		 
		 
		 loop current_hospital over:hospitals{
		 	 create medecin number: nb_doc {
			 	lieu_hospital <- current_hospital;
			 	location <- any_location_in(lieu_hospital);	
			 	est_personne<-false;
		 	}
		 	
		 }
		 
	 }
	
	reflex avancer_jour{
		write nombre_tour;
		write jour_courant;
		if(nombre_tour=240)
		{
			jour_courant<-jour_courant+1;
			//write jour_courant;
			nombre_tour <-0;
		}
		else
		{
			nombre_tour<-nombre_tour+1;
		}
		if(jour_courant>6)
		{
			if(flip(0.1))
			{
				jour_courant<-0;	
			}
		}
		
	}
	
	
}
species place{
	string type_place;
	rgb color <- #gray;
	rgb initial_color;
	float height <-0.0001+rnd(0.0002) ;
	bool est_en_quarantaine;
	bool est_traite<-false;
	aspect base{
		draw shape color:color ;
		//depth: height;
	}
	
	
	reflex changer_statut when: type_place!="hospitals"
	{
		list<personne> my_agents <- agents_inside(self);
		int personnes_agents<-0;
		int infected_agents <-0;
		loop my_agent over:my_agents{
			if(my_agent!=nil and my_agent.est_personne)
			{
				personnes_agents<-personnes_agents+1;
				if(my_agent.est_infecte=true and my_agent.color=#red)
				{
					infected_agents <-infected_agents+1;
				}
			}
		}
		if(infected_agents>=seuil_quarantaine)
		{
			//write infected_agents;
			if(infected_agents>=0.5*personnes_agents)
			{
				color<-quarantaine_color;
				est_en_quarantaine<-true;
				//bloquer les agents de la zone
				loop my_new_agent over:my_agents{
					if(my_new_agent!=nil and my_new_agent.est_personne)
					{
						my_new_agent.est_bloque<-true;
						my_new_agent.zone_urgence<-self;	
					}
				}
			}
			else
			{
				if(infected_agents=0)
				{
					color<-initial_color;
					est_en_quarantaine<-false;
					est_traite<-false;
					//liberer les personnes et les docteurs
					loop my_new_agent over:my_agents{
						if(my_new_agent!=nil)
						{
							my_new_agent.est_bloque<-false;	
							my_new_agent.a_urgence<-false;	
							my_new_agent.zone_urgence<-nil;	
						}
							
					}
				}
			}
		}
		else
		{
			if(infected_agents=0)
			{
				color<-initial_color;
				est_en_quarantaine<-false;	
				est_traite<-false;
			}
		}		
	} 
}

species ville {
	rgb color <- #darkgray  ;
	aspect base {
		draw shape color:color;
	}
	
}

/*
 * @species rue
 */
 
species rue {
	aspect base{
		draw shape ;
	}
}

/*
 * @sepecies medecin
 */
 species medecin skills:[moving] parent:personne{
	rgb color<-#olivedrab;
	place lieu_hospital;
	float rayon_guerrir<-3.0;
	bool est_occupe<-false;
	point target;
	float my_size<-0.000007;
	bool arreter<-false;
	aspect base {
		draw circle(my_size) color: color;
	}
	
	reflex retourner_hospital when:a_urgence=false and est_occupe=false{
		bool continue_walking_on_road<-true;
		//list<medecin> medecins <- agents_inside(chemins);
		/*loop current_medecin over: medecins{
			if(current_medecin!=nil)
			{
				if(!current_medecin.est_personne)
				{
					if(current_medecin=self)
					{	
						continue_walking_on_road<-true;
						break;		
					}
				}
				
			}
			
		}*/	
		
		
		if(continue_walking_on_road)
		{
			do goto target:any_location_in(lieu_hospital) on: chemins speed:0.0005;
			
		}
		else
		{
			//do goto target:any_location_in(lieu_hospital);
			arreter<-true;
				
		}
	}
	
	reflex aller_en_urgence when:a_urgence{
		if(zone_urgence!=nil)
		{
			do goto target:any_location_in(zone_urgence) on: chemins speed:0.0005 ;	
		}
	}
	
	
	reflex aller_vers_malade{
		personne personne_a_guerrir;
		//write zone_guerison;
		string place_to_ask<-"hospital";
		if(a_urgence)
		{
			place lieu_guerrir;
			if(zone_urgence!=nil){
				lieu_guerrir<-zone_urgence;
			}
			else
			{
				lieu_guerrir<-lieu_hospital;
			}
			if(lieu_guerrir!=nil)
			{
				ask lieu_guerrir{
					list<personne> patients <- agents_inside(self);
					loop patient over:patients{
						if(patient!=nil)
						{
							if patient.est_personne{
								if patient.est_infecte and patient.color=#red{
									 personne_a_guerrir<-patient;
								}
								break;
							}
						}
						
					}
				}
				do guerrir(personne_a_guerrir);
			}
		}
		
	}
	
	action guerrir (personne personne_a_guerrir) {
	    // statements...
	     if(personne_a_guerrir!=nil)
		{
		 	bool continue<-false;
		 	//target<-personne_a_guerrir.location ;
		 	if(personne_a_guerrir.est_prise_en_charge)
		 	{
		 		if(target=personne_a_guerrir.location)
		 		{
		 			continue<-true;
		 			if(location!=personne_a_guerrir.location)
		 			{
		 				target<-personne_a_guerrir.location ; 	
			    		do goto target: target ;
		 			}
		 			
		 		}
		 	}
		 	else
		 	{
		 		est_occupe<-true;
		 		target<-personne_a_guerrir.location ; 	
			    do goto target: target ;
		 		continue<-true;
		 	}
		 	if continue=true{
			 	ask personne_a_guerrir{
					est_prise_en_charge<-true; 	
			 		vitesse_guerrir<-vitesse_guerrir-myself.rayon_guerrir;
		 			if(vitesse_guerrir<=0)
		 			{
		 				write "une personne guerrie";
		 				color<-#black;
		 				myself.target<-nil;
		 				myself.est_occupe<-false;
		 				//write "top";
		 				est_infecte<-false;
		 				est_prise_en_charge<-false;
		 				vitesse_guerrir<-initial_vitesse_guerrir;
		 				nombre_guerrison<-nombre_guerrison+1;
		 				
		 			}
			 	}	
		 	}
		 	
		 }
	    
	}	
}

/*
 * @species personne
 */
 
 species personne skills:[moving]{
	int age<-10+rnd(70);
	float niveau_sante<-10/age+10;
	bool est_infecte<-false;
	float niveau_malade<-1.0;
	float initial_vitesse_guerrir<-niveau_sante/niveau_malade;
	float vitesse_guerrir<-initial_vitesse_guerrir;
	rgb color<-#black;
	int duree_maison<-60+rnd(10);
	int duree_rester_lieu<-duree_maison;
	int duree_travail<-70+rnd(10);
	place lieu_travail;
	place lieu_maison;
	place lieu_loisir;	
	int place_courante<-0;
	float rayon_infecte<-0.00002;
	bool est_prise_en_charge<-false;
	bool est_personne<-true;
	bool est_bloque<-false;
	bool a_urgence<-false;
	place zone_urgence;
    point target;
    float size<-0.000009;
	
	aspect base {
		draw circle(size) color: color ;
	}
	
	reflex patroling when: est_bloque=false {
		do goto target: target speed:0.0005 on: chemins;
	}
	
	reflex tomber_malade when: est_infecte=true and est_prise_en_charge=false{
		color<-#sienna;
		niveau_malade<-niveau_malade+0.1;
		if(niveau_malade>=0.5*(niveau_sante+10))
		{
			color<-#red;
		}
	}
	reflex mourir when:est_infecte=true and color=#red{
		niveau_malade<-niveau_malade+0.1;
		if(niveau_malade>=niveau_sante+100)
		{
			do die;
			nombre_mort<-nombre_mort+1;
		}
	}
	
	reflex quitter_lieu when: duree_rester_lieu <=0 and color!=#red {
		
		//do wander amplitude:100.0 speed:0.0003;
		if(place_courante=0)
		{
			place_courante<-1;
		}
		else
		{
			place_courante<-0;
		}
		if(place_courante=1)
		{
			if(jour_courant>=6)
			{
				//aller vers lieu de loisir
				if(lieu_loisir != nil )
				{
					if(lieu_loisir.est_en_quarantaine=false)
					{
						target<-any_location_in(lieu_loisir);
						if(location=target)
						{
							place_courante<-1;
						}	
					}
				}
			}
			else
			{
			
				if(lieu_travail!=nil and lieu_travail.type_place="ecole" )
				{
					//if(lieu_ecole.est_en_quarantaine=false)
					//{
						//write "ecole";
						duree_rester_lieu<-duree_travail;
						target<-any_location_in(lieu_travail) ;
						do goto target: target speed:0.0006 ;
						if(location=target)
						{
							place_courante <-1;
							//write place_courante;
						}	
					//}
					
				}
				if(lieu_travail!=nil and lieu_travail.type_place="travail")
				{
					if( lieu_travail.est_en_quarantaine=false)
					{
						//write "travail";
						duree_rester_lieu<-duree_travail;
						target<-any_location_in(lieu_travail);
						do goto target: target  speed:0.0006;
						if(location=target)
						{
							place_courante <- 1;
						}	
					}
					
				}
							
			}
			
		}
		else
		{
			if(lieu_maison!=nil)
			{
				if(lieu_maison.est_en_quarantaine=false)
				{
					target<-any_location_in(lieu_maison);
					duree_rester_lieu<-duree_maison;
					
					if(location=target)
					{
						//write "good";
						place_courante<-0;	
					}	
				}		
			}
			
		}
	} 
	
	reflex aller_hopital when: est_infecte=true and color=#red
	{
		if(flip(0.01))
		{
			if(place_courante!=2)
			{
				list hospitals <- place where (each.type_place="hospital");
			    target<-any_location_in(one_of(hospitals));
			    place_courante<-2;	
			}	
		}
		
	} 
		
	reflex decompte_duree_rester_lieu when: duree_rester_lieu>0 and location=target
	{
		//do wander amplitude:5.0;
		duree_rester_lieu <- duree_rester_lieu-1;
		//write duree_rester_lieu;
	}
	
	reflex propager_maladie when: est_infecte=true{
		ask self neighbors_at rayon_infecte
		{
			if(est_infecte=false)
			{
				if(age<60)
			 	{
			 		if(flip(0.01))
			 		{
			 			est_infecte<-true;
			 		}
			 	}
			 	if(age>60)
			 	{
			 		if(flip(0.1))
			 		{
			 			est_infecte<-true;
			 		}
			 	}	
			}
			
		 	
		}
	}
	
	//une personne fait appel aux hopitaux qannd il est bloqué dans une zone
	reflex appeler_au_secours when:est_bloque=true and est_personne=true and zone_urgence!=nil
	and zone_urgence.est_traite=false{
		
		list<place> hospitals<- place where (each.type_place="hospital");
		place current_hospital<-one_of(hospitals);
		ask current_hospital
		{
			list<medecin> agents_hospital <- agents_inside(current_hospital);
			int i<-0;
			loop agent_hospital over:agents_hospital{
				if(agent_hospital!=nil)
				{
					if(!agent_hospital.est_personne)
					{
						if(!agent_hospital.a_urgence)
						{	
							agent_hospital.zone_urgence<-myself.zone_urgence;
							agent_hospital.a_urgence<-true;
							i<-i+1;
							if(i=nombre_medecin_urgence)
							{
								myself.zone_urgence.est_traite<-true;
								break;
							}
								
						}
						
					}
							
				}
			}
		}
		
	
	}
	
}
experiment epidemie type: gui {
	parameter "Shapefile des places:" var: places_shape_file category: "GIS" ;
	parameter "Shapefile de la rue:" var: rues_shape_file category: "GIS" ;
		
	output {
		display city_display type:opengl {
			species ville aspect:base;
			species rue aspect:base;
			species place aspect:base;
			species personne aspect: base;
			species medecin aspect: base;
		}
		
		/*add monitor */ 	
		monitor "nombre jour " value: jour_courant color:#gray;
		monitor "personne_non_infecte" value: personne count(each.est_infecte=false) color:#green;
		monitor "Personne infectee" value: personne count(each.est_infecte=true and color!=#red) color:#orange ;
		monitor "Personne malade" value: personne  count(each.est_infecte = true and color=#red) color:#red;
		monitor "Personne mort" value: nombre_mort color:#black;
		monitor "personne infecte moins de 25 ans" value: personne count(each.est_infecte and each.age < 25 ) color:#purple ;
		monitor "people infecte entre 25 et 60 ans" value: personne count(each.est_infecte and 25 <= each.age and each.age < 70) color:#purple ;
		monitor "personne infecte plus de 60 ans " value: personne count(each.est_infecte and each.age > 70) color:#purple ;
		
		//
		display BuildingsDistribution{
			
			chart "buildings with type distribution" type: pie{
				data "Maison" value: length(place where (each.type_place="maison"))  color:#indigo;
				data "Lieux de Travail" value: length(place where (each.type_place="travail"))  color:#gray;
				data "ecole" value:length(place where (each.type_place="ecole")) color:#green;
				data "Hopitaux" value:length(place where (each.type_place="hospital")) color:#darkslategray;
				data "Losirs" value:length(place where (each.type_place="loisir")) color:#olive;
				
			}
		}
	}
}