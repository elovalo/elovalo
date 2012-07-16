################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init

RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include subdir.mk
-include objects.mk

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
ifneq ($(strip $(ASM_DEPS)),)
-include $(ASM_DEPS)
endif
ifneq ($(strip $(S_DEPS)),)
-include $(S_DEPS)
endif
ifneq ($(strip $(S_UPPER_DEPS)),)
-include $(S_UPPER_DEPS)
endif
endif

-include ../makefile.defs

# Add inputs and outputs from these tool invocations to the build variables 
LSS += \
ledivilkku2.lss \

FLASH_IMAGE += \
ledivilkku2.hex \

EEPROM_IMAGE += \
ledivilkku2.eep \

SIZEDUMMY += \
sizedummy \


# All Target
all: ledivilkku2.elf secondary-outputs

# Tool invocations
ledivilkku2.elf: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: AVR C Linker'
	avr-gcc -Wl,-Map,ledivilkku2.map -mmcu=atmega328p -o "ledivilkku2.elf" $(OBJS) $(USER_OBJS) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

ledivilkku2.lss: ledivilkku2.elf
	@echo 'Invoking: AVR Create Extended Listing'
	-avr-objdump -h -S ledivilkku2.elf  >"ledivilkku2.lss"
	@echo 'Finished building: $@'
	@echo ' '

ledivilkku2.hex: ledivilkku2.elf
	@echo 'Create Flash image (ihex format)'
	-avr-objcopy -R .eeprom -O ihex ledivilkku2.elf  "ledivilkku2.hex"
	@echo 'Finished building: $@'
	@echo ' '

ledivilkku2.eep: ledivilkku2.elf
	@echo 'Create eeprom image (ihex format)'
	-avr-objcopy -j .eeprom --no-change-warnings --change-section-lma .eeprom=0 -O ihex ledivilkku2.elf  "ledivilkku2.eep"
	@echo 'Finished building: $@'
	@echo ' '

sizedummy: ledivilkku2.elf
	@echo 'Invoking: Print Size'
	-avr-size --format=avr --mcu=atmega328p ledivilkku2.elf
	@echo 'Finished building: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(OBJS)$(C_DEPS)$(ASM_DEPS)$(EEPROM_IMAGE)$(FLASH_IMAGE)$(ELFS)$(LSS)$(S_DEPS)$(SIZEDUMMY)$(S_UPPER_DEPS) ledivilkku2.elf
	-@echo ' '

secondary-outputs: $(LSS) $(FLASH_IMAGE) $(EEPROM_IMAGE) $(SIZEDUMMY)

.PHONY: all clean dependents
.SECONDARY:

-include ../makefile.targets
