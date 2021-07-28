def calculate_degree(polynomial):
    return len(polynomial)-1

def initialise_crc_register(degree_of_polynomial):
    return [0]*degree_of_polynomial

def augment_message(message,degree_of_polynomial):
    return message+[0]*degree_of_polynomial

def list_xor(list_a,list_b):
    return list(a^b for a,b in zip(list_a,list_b))

def most_significant_bit(register):
    return register[0]

def update_crc_register(crc_register,message):
    return crc_register[1:]+[message[0]]

def do_crc(message,crc_register,polynomial):
    if not message:
        print(crc_register)
    else:
        crc_register_msb =  most_significant_bit(crc_register)
        updated_crc_register = update_crc_register(crc_register,message)
        xored_register = list_xor(updated_crc_register, polynomial[1:])
        print(crc_register)
        if crc_register_msb == 1:
            do_crc(message[1:],xored_register,polynomial)
        else:
            do_crc(message[1:],updated_crc_register,polynomial)
            

def simple_crc(polynomial,message):
    degree_of_polynomial = calculate_degree(polynomial)
    crc_register = initialise_crc_register(degree_of_polynomial)
    augmented_message = augment_message(message,degree_of_polynomial)
    do_crc(augmented_message,crc_register,polynomial)
