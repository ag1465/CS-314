/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Fall 2017                                *
 *  Author: Uli                              *
 *  Student Version                          *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	Instruction *first, *second, *third, *tempsecond, *tempthird;
	Instruction *cur = head;
			
	while(cur != NULL && cur->next != NULL && cur->next->next != NULL){
		first = cur;
		second = cur->next;
		third = cur->next->next;
		if(first->opcode == LOADI && second->opcode == LOADI){
			switch(third->opcode){
				case ADD:
					if((first->field2 == third->field1 && second->field2 == third->field2)||(first->field2 == third->field2 && second->field2 == third->field1)){
						cur->field2 = third->field3;
						cur->field1 = first->field1 + second->field1;
						cur->next = third->next;
						if(third->next != NULL){
						third->next->prev = cur;
						}
						tempsecond = second;
						tempthird = third;
						free(tempsecond);
						free(tempthird);
						}
					break;
				case MUL:
					if((first->field2 == third->field1 && second->field2 == third->field2)||(first->field2 == third->field2 && second->field2 == third->field1)){
						cur->field2 = third->field3;
						cur->field1 = first->field1 * second->field1;
						cur->next = third->next;
						if(third->next != NULL){
						third->next->prev = cur;
						}
						tempsecond = second;
						tempthird = third;
						free(tempsecond);
						free(tempthird);
						}
					break;
				case SUB:
					if(first->field2 == third->field1 && second->field2 == third->field2){
						cur->field2 = third->field3;
						cur->field1 = first->field1 - second->field1;
						cur->next = third->next;
						if(third->next != NULL){
						third->next->prev = cur;
						}
						tempsecond = second;
						tempthird = third;
						free(tempsecond);
						free(tempthird);
						}
					break;
				default:
					break;
			}
		}
		cur = cur->next;
		}

	if (head) 
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	return EXIT_SUCCESS;
}

