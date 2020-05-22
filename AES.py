import numpy as np
import struct

## Function
def Sub_Bytes(state):
    ## Melakukan Sub Bytes pada state yang diinput dengan s-box yang disediakan
    ## Input : Array 4x4
    ## Output: Array 4x4

    ## Deklarasi S-box
    sbox = [[0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
            [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
            [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
            [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
            [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
            [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
            [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
            [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
            [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
            [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
            [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
            [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
            [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
            [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
            [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
            [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]]


    newState = []
    for i in range(len(state)):
        temp = []
        for j in range(len(state[i])):
            index = divmod(state[i][j], 0x10) ## Memisahkan 1 bytes menjadi 2 bytes yang berbeda
            temp.append( sbox[index[0]][index[1]] ) ## Mengambil S-box( bytes-1, bytes-2 ), lalu push ke temp
        newState.append(temp)
            
    return newState

def Shift_Row(state):
    ## Melakukan Shift Row pada state yang diinput
    ## Input : Array 4x4 
    ## Output: Array 4x4 

    ## Mengubah kolom menjadi baris dan sebaliknya pada state
    state = np.array(state).transpose()
    state = state.tolist()
    
    newState = []
    for j in range(len(state)):
            newState.append( state[j][j:]+state[j][:j] ) ## melakukan shift chiperkey ke kiri sesuai i, i.g. i = 1 [0x1, 0x2, 0x3] menjadi [0x2, 0x3, 0x1]
    
    ## Mengubah kolom menjadi baris dan sebaliknya pada newState
    newState = np.array(newState).transpose()
    newState = newState.tolist()
    
    return newState

def Mix_Column(state):
    ## Melakukan Mix Column pada state yang diinput dengan mix column yang disediakan
    ## Input : Array 4x4 
    ## Output: Array 4x4   

    ## Deklarasi mix Column
    mixColumn = [[0x02, 0x01, 0x01, 0x03],
                [0x03, 0x02, 0x01, 0x01],
                [0x01, 0x03, 0x02, 0x01],
                [0x01, 0x01, 0x03, 0x02]]
                
    ## Deklarasi Polynomial Irreducible dalam binner, irr = x^8 + 1 = 100000001 = 257
    irr = np.poly1d( [1,0,0,0,0,0,0,0,1] )

    mixColumn = np.array(mixColumn).transpose()

    newState = []
    for k in range(len(state)):
        tempNewState = []
        for j in range(len(mixColumn)):    
            tempState = []
            for i in range(len(mixColumn[j])):
                p1 = GF28(state[k][i]) ## Mengubah state(i,j) menjadi array berisi binner, i.g. 5 menjadi [0,1,0,1]
                p2 = GF28(mixColumn[j][i]) ## mengubah mix column(i,j) menjadi array berisi binner
                
                p = np.polymul(p1, p2) ## Perhitungan p1 . p2, lalu tampung di p
                p = np.polydiv(p, irr)[1] ## Melakukan modulo polynomail p dan irr
                p = list( map(lambda x: 0 if x%2==0 else 1, p) ) ## Mengubah bilangan genap menjadi 0 dan ganjil menjadi 1, pada array p

                ## Mengubah array binner menjadi binner, i.g. [0,1,0,1] menjadi 0b0101
                newP = ["0b"]
                for h in range(len(p)):
                    newP.append(str(int(p[h])))
                tempState.append(int((''.join(newP)), 2))
            
            total = 0
            for item in tempState:
                total = total ^ item ## Perhitungan total(i) xor tempState(i)
            tempNewState.append(total)

        newState.append(tempNewState)

    return newState

def Add_Round_key(state, roundKey):
    ## Menjumlahkan RoundKey pada state yang diinput dengan Roundkey yang diinput
    ## Input : Array 4x4, Array 4x4
    ## Output: Array 4x4 

    newState = []
    for j in range(len(state)):
        tempState = []
        for i in range(len(state[j])):
            tempState.append(state[j][i] ^ roundKey[j][i]) ## Perhitungan state(j,i) xor roundKey(j,i), lalu push ke tempstate
        newState.append(tempState) ## Push tempState ke newState

    return newState

def Key_Expansion(chiperKey, size):
    ## Membuat key berdasarkan state yang diinput sebanyak size + 1
    ## Input : Array 4x4, int
    ## Output: Array 4x4 
    
    ## Deklarasi S-Box
    sbox = [[0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
            [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
            [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
            [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
            [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
            [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
            [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
            [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
            [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
            [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
            [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
            [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
            [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
            [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
            [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
            [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]]

    size += 2

    ## Membuat Rcon sebanyak size

    rc = [0x01] ## Deklarasi rc(0)
    rcon = [[rc[0], 0, 0, 0]] ## Deklarasi Rcon(0) = [ rc(0), 0, 0, 0 ]
    for i in range(1, size):
        p1 = GF28(rc[i-1])
        p2 = GF28(0x2)
        
        p = np.polymul(p2, p1) ## Perkalian p2 dengan p1

        ## Mengubah array binner menjadi binner, i.g. [0,1,0,1] menjadi 0b0101
        newP = ["0b"]
        for h in range(len(p)):
            newP.append(str(int(p[h])))
        newP = int((''.join(newP)), 2)

        ## Jika panjang bit lebih dari 8, hitung rc xor 0x11b
        if newP.bit_length() > 8:
            newP = newP ^ 0x11b
        
        rcon.append([newP, 0, 0, 0]) ## Push rc ke dalam Rcon(i) = [rc, 0, 0, 0]
        rc.append(newP)
    ## Perhitungan Rcon berakhir disini

    ## Proses pembangkitan kunci sebanyak size
    for i in range(4, (size*4)):
        if i % 4 == 0 :

            chiperKey.append( chiperKey[i-1][1:]+chiperKey[i-1][:1] ) ## melakukan shift row chiperkey ke kiri sesuai i, i.g. i = 1, [0x1, 0x2, 0x3] menjadi 0x2, 0x3, 0x1]

            temp = []
            for j in range(len(chiperKey[i])):
                index = divmod(chiperKey[i][j], 0x10) ## Memisahkan 1 bytes menjadi 2 bytes yang berbeda
                temp.append( sbox[ index[0] ][ index[1] ] ) ## Mengambil S-box( bytes-1, bytes-2 ), lalu push ke temp
            chiperKey[i] = temp

            temp = []
            for j in range(len(chiperKey[i])):
                chiperKey[i][j] = chiperKey[i][j] ^ chiperKey[i-4][j] ^ rcon[int(i/4)-1][j] ## Perhitungan chiperKey(i,j) xor chiperKey(i-4,j) xor rcon(i/4 -1,j)
        else:
            temp = []
            for j in range(len(chiperKey[i-1])):
                temp.append( chiperKey[i-1][j] ^ chiperKey[i-4][j] ) ## Perhitungan chiperKey(i-1,j) xor chiperKey(i-4,j)

            chiperKey.append(temp)

    # memisahkan roundKey menjadi array 4x4
    roundKey = []
    for i in range(0, len(chiperKey)-4, 4):
        roundKey.append(chiperKey[i:i+4])

    return roundKey

## Function Inverse
def Inv_Sub_Bytes(state):
    ## Melakukan Inverse Sub Bytes terhadap state dengan Inverse S-Box
    ## Input : Array 4x4 
    ## Output: Array 4x4     

    ## Deklarasi Inverse S-box
    inv_sbox = [[0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb],
                [0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb],
                [0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e],
                [0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25],
                [0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92],
                [0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84],
                [0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06],
                [0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b],
                [0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73],
                [0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e],
                [0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b],
                [0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4],
                [0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f],
                [0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef],
                [0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61],
                [0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d]]

    newState = []
    for i in range(len(state)):
        temp = []
        for j in range(len(state[i])):
            index = divmod(state[i][j], 0x10) ## Memisahkan 1 bytes menjadi 2 bytes yang berbeda
            temp.append( inv_sbox[ index[0] ][ index[1] ] ) ## Mengambil S-box( bytes-1, bytes-2 ), lalu push ke temp
        newState.append(temp)
            
    return newState

def Inv_Shift_Row(state):
    ## Melakukan Inverse Shift Row terhadap state
    ## Input : Array 4x4 
    ## Output: Array 4x4 

    ## Mengubah kolom menjadi baris dan sebaliknya pada state
    state = np.array(state).transpose()
    state = state.tolist()
    
    newState = []
    for j in range(len(state)):
        newState.append( state[j][4-j:]+state[j][:4-j] ) ## melakukan shift chiperkey ke kanan sesuai i, i.g. i = 1 [0x1, 0x2, 0x3] menjadi [0x3, 0x1, 0x2]
    
    ## Mengubah kolom menjadi baris dan sebaliknya pada newState
    newState = np.array(newState).transpose()
    newState = newState.tolist()
    
    return newState

def Inv_Mix_Column(state):
    ## Melakukan Inverse Mix Column terhadap state dengan Inverse Mix Column yang disediakan
    ## Input : Array 4x4
    ## Output: Array 4x4

    ## Deklarasi inverse mix Column
    inv_mixColumn = [[0x0e, 0x09, 0x0d, 0x0b],
                    [0x0b, 0x0e, 0x09, 0x0d],
                    [0x0d, 0x0b, 0x0e, 0x09],
                    [0x09, 0x0d, 0x0b, 0x0e]]
    
    ## Deklarasi Polynomial Irreducible dalam binner, irr = x^8 + 1
    irr = np.poly1d( [1,0,0,0,0,0,0,0,1] )

    inv_mixColumn = np.array(inv_mixColumn).transpose()

    newState = []
    for k in range(len(state)):
        tempNewState = []
        for j in range(len(inv_mixColumn)):    
            tempState = []
            for i in range(len(inv_mixColumn[j])):
                p1 = GF28(state[k][i]) ## Mengubah state(i,j) menjadi array berisi binner, i.g. 5 menjadi [0,1,0,1]
                p2 = GF28(inv_mixColumn[j][i]) ## mengubah inverse mix column(i,j) menjadi array berisi binner
                
                p = np.polymul(p1, p2) ## Perhitungan p1 . p2, lalu tampung di p
                p = np.polydiv(p, irr)[1] ## Melakukan modulo polynomail p dan irr
                p = list( map(lambda x: 0 if x%2==0 else 1, p) ) ## Mengubah bilangan genap menjadi 0 dan ganjil menjadi 1, pada array p

                ## Mengubah array binner menjadi binner, i.g. [0,1,0,1] menjadi 0b0101
                newP = ["0b"]
                for h in range(len(p)):
                    newP.append(str(int(p[h])))

                tempState.append(int((''.join(newP)), 2))
            
            total = 0
            for item in tempState:
                total = total ^ item
            tempNewState.append(total) ## Perhitungan total(i) xor tempState(i)

        newState.append(tempNewState)

    return newState

## Fungsi Lainya
def GF28(number):
    ## Merubah ankga menjadi polynomial dengan GF(8^2), i.g. x^3 + x + 1 adalah [1,0,1,1]
    ## Input : Integer
    ## Output: Array

    binary = "{0:08b}".format(number)
    return list(map(int, binary))

def Conv_Str_to_ASCII(text):
    ## Mengubah text menjadi ASCII sepanjgan 16 bytes, ditampung dalam array 4x4 dipisahkan sebanyak 4 bytes setiap kolom
    ## i.g. 'irfan' menjadi [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 105], [114, 102, 97, 110]]
    ## Input : String dengan max len = 16
    ## Output: Array 4x4

    text = [ord(c) for c in text]

    while len(text) < 16:
        text = [0x00] + text
    state = [text[i:i+4] for i in range(0, len(text), 4)]

    return state

def Conv_ASCII_to_Str(chiperText):
    ## Mengubah Angka menjadi string, dengan menggunakan ASCII
    ## i.g. [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 105], [114, 102, 97, 110]] menjadi 'irfan'
    ## Input : Array 4x4
    ## Output: String

    chiperText = np.hstack( np.array(chiperText) ).tolist()
    chiperText = list(filter(lambda x : x != 0, chiperText))
    
    text = ''.join(chr(i) for i in chiperText)

    return str(text)

def print_to_Hex(state):
    ## Show state sebagai hexadecimal, dengan format string 0xFF, i.g. [14, 5, 10] menjadi 0xe 0x5 0xa
    ## Input : array 0d/1d/2d/3d

    matrix = np.array(state)

    if matrix.ndim == 3:
        for i in range(len(matrix)):
            matrix[i] = matrix[i].transpose()

        for k in range(len(matrix)):
            for i in range(len(matrix[k])):
                for item in matrix[k][i]:
                    print(hex(item), end=' ')
                print()
            print()
    elif matrix.ndim == 2:
        matrix = matrix.transpose()

        for i in range(len(matrix)):
            for item in matrix[i]:
                print(hex(item), end=' ')
            print()
    elif matrix.ndim == 1:
        for item in matrix:
            print(hex(item), end=' ')
        print()
    elif matrix.ndim == 0:
        print(hex(matrix))

    print()

## Function Enkripsi dan Dekripsi
def enkripsi():
    ## Proses Enkripsi
    ## Input : Global variabel
    ## Output: String chiperText

    global chiperText
    global roundKey
    global N
    
    print('Enkripsi ------------------------------------------------------------------------------------------------------------------')
    print()

    ## Show input
    print('input')
    print_to_Hex(chiperText)
    
    ## Add Round Key pada proses N = 0, dengan Round Key ke 0
    state = Add_Round_key(chiperText, roundKey[0])
    print('Add RK    ', 0)
    print_to_Hex(state)

    ## Enkripsi N = 1 hingga N-1
    for i in range(1, N):

        ## Sub Bytes
        state = Sub_Bytes(state)
        print('Sub Bytes ', i)
        print_to_Hex(state)        

        ## Shift Row
        state = Shift_Row(state)
        print('Shift Row ', i)
        print_to_Hex(state)        

        ## Mix Column
        state = Mix_Column(state)
        print('Mix Column', i)
        print_to_Hex(state)        

        ## Add Round Key, dengan Round Key ke i
        state = Add_Round_key(state, roundKey[i])
        print('Add RK    ', i)
        print_to_Hex(state)

    ## Perhitungan Enkripsi ke N

    ## Sub Bytes
    state = Sub_Bytes(state)
    print('Sub Bytes ', N)
    print_to_Hex(state)
    
    ## Shift Row
    state = Shift_Row(state)
    print('Shift Row ', N)
    print_to_Hex(state)
    
    ## Add Round key, dengan Round Key ke N
    state = Add_Round_key(state, roundKey[N])
    print('Add RK    ', N)
    print_to_Hex(state)

    ## Show output
    print('Output')
    print_to_Hex(state)
    print()

    chiperText = state

def dekripsi():
    ## Proses Dekripsi
    ## Input : Global variabel
    ## Output: String chiperText

    global chiperText
    global roundKey
    global N
    
    print('dekripsi ------------------------------------------------------------------------------------------------------------------')
    print()

    ## Show input
    print('input')
    print_to_Hex(chiperText)
    
    ## proses N = 0
    ## Add Round key, dengan Round key ke N
    state = Add_Round_key(chiperText, roundKey[-1])
    print('Add RK    ', 0)
    print_to_Hex(state)    

    ## Shift Row
    state = Inv_Shift_Row(state)
    print('Shift Row ', 0)
    print_to_Hex(state)

    ## Sub Bytes
    state = Inv_Sub_Bytes(state)
    print('Sub Bytes ', 0)
    print_to_Hex(state)

    ## Proses Dekripsi N = 1 hingga N-1
    for i in range(1, N):

        ## Add Round Key dengan Round key ke N-i
        state = Add_Round_key(state, roundKey[-i-1])
        print('Add RK    ', i)
        print_to_Hex(state)

        ## Inverse Mix Column
        state = Inv_Mix_Column(state)
        print('Mix Column', i)
        print_to_Hex(state)

        ## Inverse Shift Row
        state = Inv_Shift_Row(state)
        print('Shift Row ', i)
        print_to_Hex(state)

        ## Inverse Sub Bytes
        state = Inv_Sub_Bytes(state)
        print('Sub Bytes ', i)
        print_to_Hex(state)

    ## Perhitungan Dekripsi ke N
    state = Add_Round_key(state, roundKey[0])
    print('Add RK    ', N)
    print_to_Hex(state)

    ## Show output
    print('Output')
    print_to_Hex(state)
    print()

    chiperText = state

## main

chiperText = Conv_Str_to_ASCII('Irfan Nugraha') ## Deklarasi chiper text, !!! len(chiper text) < 16 !!!
chiperKey = Conv_Str_to_ASCII('irfannugraha69@1') ## Deklatasi Chiper Key
N = 10 ## Deklarasi N

## Membangkitkan kunci sebanyak N + 1
roundKey = Key_Expansion(chiperKey, N)

## Enkripsi
enkripsi()
print('chiperText :', Conv_ASCII_to_Str(chiperText))

## Dekripsi
dekripsi()
print('chiperText :', Conv_ASCII_to_Str(chiperText))
