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
#include <math.h>
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

	Instruction *first, *second;
	Instruction *cur = head;	

	while(cur != NULL && cur->next != NULL){
		first = cur;
		second = first->next;
		double num = log2(first->field1);		
		if(first->opcode == LOADI && num == (int)num){
			switch(second->opcode){
				case MUL:
				if(first->field2 == second->field2){
					cur->opcode = LSHIFTI;
					cur->field1 = second->field1;
					cur->field2 = num;
					cur->field3 = second->field3;
					cur->next = second->next;
					if (second->next != NULL){
						second->next->prev = cur;
						free (second);
					}
				}
				break;
				case DIV:
				if(first->field2 == second->field2){
					cur->opcode = RSHIFTI;
					cur->field1 = second->field1;
					cur->field2 = num;
					cur->field3 = second->field3;
					cur->next = second->next;
					if (second->next != NULL){
						second->next->prev = cur;
						free (second);
					}
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
