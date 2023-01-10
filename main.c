#include <stdlib.h>
#include <stdio.h>
#include<string.h>
#include <math.h>
#include<time.h>
#include <omp.h>



unsigned char** matriz_creacion(int ancho, int largo){
    int i;
    unsigned char** matriz = (unsigned char**) malloc (largo * sizeof(unsigned char*)); //Se declara un array de punteros del tamaño de largo
    for(i=0;i<largo;i++){
        matriz[i]= (unsigned char*) malloc (ancho * sizeof(unsigned char)); //Por cada puntero del array anterior se reserva memoria para una array de unsigned chars de tamaño ancho
    }
    
    return matriz;
    
}

void matriz_load(char* fichero, int ancho, int largo, unsigned char** matrix){
    FILE *f = fopen (fichero, "rb");	
    unsigned char linea[ancho];
	int i,j;
	
	for(i=1;i<=largo;i++){
        
        fread(linea, sizeof(linea), 1, f);
        for(j=0;j<ancho;j++){
          
            matrix[i][j+1]= linea[j];            
        }
        
    }
    
    fclose(f);
   
    for(i=0;i<ancho;i++){
        matrix[0][i]=matrix[2][i];
        matrix[largo+1][i]=matrix[largo-1][i];
    }
    for(i=1;i<largo;i++){
        matrix[i][0]=matrix[i][2];
        matrix[i][ancho+1]=matrix[i][ancho+1];
    }
    matrix[0][0]=matrix[2][2];
    matrix[0][ancho+1]=matrix[2][ancho-1];
    matrix[largo+1][0]=matrix[largo-1][2];
    matrix[largo+1][ancho+1]=matrix[largo-1][ancho-1];   

}




unsigned char media (unsigned char ** matriz, int x, int y){
    int media=0;
    unsigned char resultado;
    //esquina sup iz
    media+=  matriz[x-1][y-1] ;
    //mitad superior
    media += matriz[x][y-1];
    //esquina sup der
    media+= matriz[x+1][y-1] ;
    //mitad izq
    media+= matriz[x-1][y] ;
     //elemento
    media+= matriz[x][y] ;
    //mitad derecha
    media+= matriz[x+1][y];
    //esquina inf izq
    media+= matriz[x-1][y+1] ;
    //centro inf
    media+= matriz[x][y+1] ;
    //esq inf der
    media+= matriz[x+1][y-1] ;
    resultado=media/9;
    return resultado;
}

void media_calculate(unsigned char** matriz, unsigned char** matriz_2, int ancho, int largo, int iam, int div){
    int i,j;

    for(i=iam*div;i<largo;i++){ //Recorre la matriz por filas ignorando la primera y la ultima.
        for(j=0;j<ancho;j++){ //Recorre cada fila elemento a elemento.
                matriz_2[i][j]=media(matriz,i+1,j+1);
        }
    }

}

void qs(unsigned char lista[],int limite_izq,int limite_der){

    int izq,der;
    unsigned char temporal,pivote;
    izq=limite_izq;
    der = limite_der;
    pivote = lista[(izq+der)/2];
 
    do{
        while(lista[izq]<pivote && izq<limite_der)izq++;
        while(pivote<lista[der] && der > limite_izq)der--;
        if(izq <=der)
        {

            temporal= lista[izq];
            lista[izq]=lista[der];
            lista[der]=temporal;
            izq++;
            der--;


        }
 
    }while(izq<=der);
    if(limite_izq<der){qs(lista,limite_izq,der);}
    if(limite_der>izq){qs(lista,izq,limite_der);}

 
}

unsigned char mediana(unsigned char ** matriz,int x, int y){
    unsigned char elementos[9];

    //Se almacenan los 9 elementos en un array para llamar a qs
    //esquina sup iz
    elementos[0]= matriz[x-1][y-1];
    //mitad superior
    elementos[1]= matriz[x][y-1];
    //esquina sup der
    elementos[2]= matriz[x+1][y-1];
    //mitad izq
    elementos[3]= matriz[x-1][y];
     //elemento
    elementos[4]= matriz[x][y];
    //mitad derecha
    elementos[5]= matriz[x+1][y];
    //esquina inf izq
    elementos[6]= matriz[x-1][y+1];
    //centro inf
    elementos[7]= matriz[x][y+1];
    //esq inf der
    elementos[8]= matriz[x+1][y-1];

    qs(elementos,0,8);//qs organiza los elementos del array

    return elementos[4];//cogemos el elemento de en medio
   

}

void mediana_calculate(unsigned char** matriz, unsigned char** matriz_2, int ancho, int largo, int iam, int div){

    int i,j;

    for(i=iam*div;i<largo;i++){//Recorre la matriz por filas ignorando la primera y la ultima.
        for(j=0;j<ancho;j++){//Recorre cada fila elemento a elemento.
                matriz_2[i][j]=mediana(matriz,i+1,j+1);

        }
    }   

}




unsigned char sobel(unsigned char** matriz,int x, int y){
     int c=0;
     int f=0;
     unsigned char j;
     //esquina sup iz
    f= matriz[x-1][y-1]*-1;
    c= matriz[x-1][y-1]*-1;
    //mitad superior
    f= f+ matriz[x][y-1]*0;
    c= c+ matriz[x][y-1]*-2;
    //esquina sup der
    f= f+ matriz[x+1][y-1]*1;
    c= c+ matriz[x+1][y-1]*-1;
    //mitad izq
    f= f+ matriz[x-1][y]*-2;
    c= c+ matriz[x-1][y]*0;
     //elemento
    f= f+ matriz[x][y]*0;
    c= c+ matriz[x][y]*0;
    //mitad derecha
    f= f+ matriz[x+1][y]*2;
    c= c+ matriz[x+1][y]*0;
    //esquina inf izq
    f= f+ matriz[x-1][y+1]*-1;
    c= c+ matriz[x-1][y+1]*1;
    //centro inf
    f= f+ matriz[x][y+1]*0;
    c= c+ matriz[x][y+1]*2;
    //esq inf der
    f= f+ matriz[x+1][y-1]*1;
    c= c+ matriz[x+1][y-1]*1;

    c= pow(c,2);
    f= pow(f,2);

    j=sqrt(c+f);

    return j;

}


void sobel_calculate (unsigned char** matriz, unsigned char** matriz_2, int ancho, int largo, int iam, int div){
    int i,j;

    for(i=iam*div;i<largo;i++){//Recorre la matriz por filas ignorando la primera y la ultima.
        for(j=0;j<ancho;j++){//Recorre cada fila elemento a elemento.
                matriz_2[i][j]=sobel(matriz,i+1,j+1);
        }
    }
}


int comp_fichero (char *fichero){
    FILE *f= fopen(fichero,"r");
    if (f==NULL){
        return 0;
        //Si no es un fichero devuelve 0 
    }
    fclose(f);
    return 1;
        //Si es un fichero devuelve 1
}
//Funcion que guarda la matriz resultante en un nuevo fihero raw
void img_create_raw (unsigned char** matrix, char* fichero,int ancho,int largo){
	int i,j;
    char nombre[50];
	FILE *f = fopen (fichero,"wb");
	if (f==NULL){ //Comprueba si el fichero existe
		printf("Error al crear el fichero");
		exit;
	}
	
	for (j = 0; j < largo; j++){
		fwrite(matrix[j],sizeof(unsigned char),ancho,f);
	}
	
	fclose(f);
   
}
//Fucion que imprime los resultados
void print_results(int ancho, int largo, char* fichero, double media, double mediana, double sobel, int hilos){
    char nombre[50];
    int contador=0;
    FILE *f;
    int i;
    
    sprintf(nombre,"Resultados_%dx%d.txt",ancho,largo);
     while(comp_fichero(nombre)==1){
        contador++;
        sprintf(nombre,"Resultados_%dx%d(%d).txt",ancho, largo,contador);
    }
   

    f=fopen(nombre,"w");
    fprintf(f,"Parametros de ejecucion\n\nancho: %d\n",ancho);
    fprintf(f,"largo: %d\n",largo);
    fprintf(f,"Numero de hilos %d \n",hilos);
    fprintf(f,"fichero de entrada: %s\n",fichero);
   
    fprintf(f,"\n");

    
    fprintf(f,"Tipo de filtrado: media\n");
    fprintf(f,"fichero de salida: media.raw\n");
    fprintf(f,"Tiempo de ejecuccion paralelo: %f \n",media);

    fprintf(f,"\n");

    fprintf(f,"Tipo de filtrado:  mediana\n");
    fprintf(f,"fichero de salida: mediana.raw\n");
	fprintf(f,"Tiempo de ejecuccion paralelo: %f \n",mediana);

	fprintf(f,"\n");

    fprintf(f,"Tipo de filtrado: sobel\n");
    fprintf(f,"fichero de salida: sobel.raw\n");
	fprintf(f,"Tiempo de ejecuccion paralelo: %f \n",sobel);

	fprintf(f,"\n");

    
    fprintf(f,"Tiempo  de ejecucion paralela total: %f\n",media+mediana+sobel);
   fclose(f);



    


}





int main(int argc, char* argv[]){
	//Variables

	double t1, t2, t3, t4;
	int hilos = atoi(argv[4]);
	int ancho = atoi(argv[2]);
	int largo = atoi(argv[3]);
	char* fichero = argv[1];
	int iam;
	int div= largo/hilos;
	int extra= largo%hilos;

	unsigned char** matrix= matriz_creacion(ancho+2,largo+2);
	unsigned char** media=matriz_creacion(ancho,largo);
	unsigned char** mediana=matriz_creacion(ancho,largo);
	unsigned char** sobel=matriz_creacion(ancho,largo);

	matriz_load(fichero,ancho,largo,matrix);

	t1 = omp_get_wtime();
	#pragma omp parallel num_threads(hilos) shared(matrix,media,ancho,largo,extra,div,hilos) private(iam)
	{
		
		iam=omp_get_thread_num();
		
		if(iam!=hilos-1){
			
			media_calculate(matrix,media,ancho,(iam*div)+div,iam,div);
		}
		else{
			
			media_calculate(matrix,media,ancho,(iam*div)+div+extra,iam,div);
		}

	}

	t2 = omp_get_wtime();

	#pragma omp parallel num_threads(hilos) shared(matrix,mediana,ancho,largo,extra,div,hilos) private(iam)
	{
		iam=omp_get_thread_num();
		if(iam!=hilos-1){
			
			mediana_calculate(matrix,mediana,ancho,(iam*div)+div,iam,div);
		}
		else{
			
			mediana_calculate(matrix,mediana,ancho,(iam*div)+div+extra,iam,div);
		}

	}

	t3 = omp_get_wtime();

	#pragma omp parallel num_threads(hilos) shared(matrix,sobel,ancho,largo,extra,div,hilos) private(iam)
	{
		iam=omp_get_thread_num();
		if(iam!=hilos-1){
			
			sobel_calculate(matrix,sobel,ancho,(iam*div)+div,iam,div);
		}
		else{
			
			sobel_calculate(matrix,sobel,ancho,(iam*div)+div+extra,iam,div);
		}

	}

	t4 = omp_get_wtime();
	
	img_create_raw(matrix,"matrix.raw",ancho+2,largo+2);
	img_create_raw(media,"media.raw",ancho,largo);
	img_create_raw(mediana,"mediana.raw",ancho,largo);	
	img_create_raw(sobel,"sobel.raw",ancho,largo);

	print_results(ancho,largo,fichero,t2-t1,t3-t2,t4-t3,hilos);

	free(matrix);
	free(media);
	free(mediana);
	free(sobel);
    return 0;
}