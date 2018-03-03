import os
import sys
import math
import matplotlib

import matplotlib.pyplot as plt 

#correct_rt = open(("acc_rt"+ ".csv"), "w")
#correct_rt.write('name,'+'order,'+ 'task,' + 'section,'+'direction,'+'correctness,'+'foil,'+'tone,'+'iteration,'+'trial,'+'image,'+'correct,'+'rt')
#correct_rt.write('\n')

#trial_d = open(("trials" + ".csv"), "w")
#trial_d.write('name,'+'order,'+'task,' +'section,'+'direction,'+'correctness,'+'foil,'+'tone,'+'trial,'+'correct,'+'rt')
#trial_d.write('\n')

def calculate(img_list,key_list,correct_rt,trial_d,day):
    ntrials = 32
    nkeys = 9
    trial_data = []
    
    graph_trial = []
    graph_acc = []
    graph_rt = []
    
    for img_item in img_list:
        #print img_item
        if img_item[12]=='it3' and img_item[13][0]=='k':
            #print img_item
        
        
            subject = img_item[0]
            order = img_item[1]
            task = img_item[2]
            section = img_item[3]
            trial = img_item[4]
            correctness =img_item[5]
            direction = img_item[6]
            tone = img_item[7]
            foil =img_item[8]
            iteration = img_item[12]
            time_stamp_img= float(img_item[11])

            correct_key = img_item[9]
            image = img_item[-1]
            
            #print img_item
            #print time_stamp_img
            
            pressed_keys = []
            correct = 0
            rt=''
            
            for key_item in key_list:
                
                if key_item != None:
                    #print key_item
                    time_stamp_key= float(key_item[11])
                    #print time_stamp_key

                    if time_stamp_key > time_stamp_img-0.25 and time_stamp_key < time_stamp_img+1:
                        pressed_keys += [[key_item[9],time_stamp_key]]
                        #print len(pressed_keys)
            #if pressed_keys !=[]: print pressed_keys, correct_key
                        
            for key_press in pressed_keys:
                #print key_press
                if key_press[0] == correct_key:
                    #print key_press[0]
                    correct = 1
                    rt = key_press[1]-time_stamp_img
                    #rt = math.fabs(key_press[1]-time_stamp_img)
                #else:
                    #continue
                    #rt = ''
                    #correct = 0
            
            
            key_stuff = subject, order, task, section, direction, correctness, foil, tone, iteration, trial, image, correct, rt,day
            trial_data += [key_stuff]
            
             
            for i in key_stuff[0:-1]:
                correct_rt.write(str(i) + ',')
            correct_rt.write(str(key_stuff[-1]))
            correct_rt.write('\n')
    
    for tt in range(0,ntrials):
        acc=0
        rt_t = 0
        trial_head=()
        for ts in trial_data:
            
            #print trial_head
            if int(ts[9]) == tt:
                if ts[9] !='': acc += float(ts[11])
                if ts[12] !='': rt_t += float(ts[12])
                n,ordr,tsk,sect,dire,crr,fo,to=ts[0],ts[1],ts[2],ts[3],ts[4],ts[5],ts[6],ts[7]
        trial_head= n,ordr,tsk,sect,dire,crr,fo,to
            
        avg_acc = acc/nkeys
        avg_rt = rt_t/nkeys
        trial_stuff= trial_head + (tt, avg_acc, avg_rt,day)
        
        graph_trial += [tt]
        graph_acc += [avg_acc]
        graph_rt += [avg_rt]
        
        #print trial_stuff
        
        for i in trial_stuff[0:-1]:
            trial_d.write(str(i) + ',')
        trial_d.write(str(trial_stuff[-1]) +',')
       
        #for j in disc_trial[0:-1]:
         #   trial_d.write(str(j) + ',')
        #trial_d.write(str(disc_trial[-1]))
        
        trial_d.write('\n')


   # plt.plot(graph_trial,graph_acc)
    #plt.show()
    #plt.imsave
    #plt.close
    #plt.plot(graph_trial,graph_rt)
    #plt.show()
    #plt.imsave
    #plt.close

                
                

                    
                
