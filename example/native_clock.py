import time

def current_milli_time():
    return round(time.time() * 1000)

N = 1000000
start = current_milli_time()
for i in range(N):
    a = 1+2
end = current_milli_time()
diff = end - start
print(diff)
