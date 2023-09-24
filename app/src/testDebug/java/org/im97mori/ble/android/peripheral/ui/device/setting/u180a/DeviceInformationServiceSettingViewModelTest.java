package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;
import static org.junit.Assert.assertArrayEquals;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.u2a23.SystemId;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.LinkedList;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceInformationServiceSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private DeviceInformationServiceSettingViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new DeviceInformationServiceSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
        mFakeDeviceSettingRepository = null;
        mSavedStateHandle = null;
    }

    @Test
    public void test_observeSetup_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isSystemIdSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasSystemIdDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdDataReference::set);
        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringDataReference::set);
        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringDataReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isSystemIdSupportedReference.get());
        assertNull(hasSystemIdDataReference.get());
        assertNull(hasModelNumberStringDataReference.get());
        assertNull(hasManufacturerNameStringDataReference.get());
        assertEquals("", manufacturerIdentifierReference.get());
        assertEquals("", organizationallyUniqueIdentifierReference.get());
        assertEquals("", modelNumberStringReference.get());
        assertEquals("", manufacturerNameStringReference.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isSystemIdSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasSystemIdDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdDataReference::set);
        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringDataReference::set);
        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringDataReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isSystemIdSupportedReference.get());
        assertNull(hasSystemIdDataReference.get());
        assertNull(hasModelNumberStringDataReference.get());
        assertNull(hasManufacturerNameStringDataReference.get());
        assertEquals("", manufacturerIdentifierReference.get());
        assertEquals("", organizationallyUniqueIdentifierReference.get());
        assertEquals("", modelNumberStringReference.get());
        assertEquals("", manufacturerNameStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isSystemIdSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasSystemIdDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdDataReference::set);
        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringDataReference::set);
        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringDataReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        serviceData.characteristicDataList.add(modelNumberStringCharacteristicData);

        String originalManufacturerNameString = "B";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(
                MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1
        );
        serviceData.characteristicDataList.add(manufacturerNameStringCharacteristicData);

        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isSystemIdSupportedReference.get());
        assertNull(hasSystemIdDataReference.get());
        assertTrue(hasModelNumberStringDataReference.get());
        assertTrue(hasManufacturerNameStringDataReference.get());
        assertEquals("", manufacturerIdentifierReference.get());
        assertEquals("", organizationallyUniqueIdentifierReference.get());
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isSystemIdSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasSystemIdDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdDataReference::set);
        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringDataReference::set);
        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringDataReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(
                SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1
        );
        serviceData.characteristicDataList.add(systemIdCharacteristicData);

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        serviceData.characteristicDataList.add(modelNumberStringCharacteristicData);

        String originalManufacturerNameString = "B";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(
                MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1
        );
        serviceData.characteristicDataList.add(manufacturerNameStringCharacteristicData);

        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isSystemIdSupportedReference.get());
        assertTrue(hasSystemIdDataReference.get());
        assertTrue(hasModelNumberStringDataReference.get());
        assertTrue(hasManufacturerNameStringDataReference.get());
        assertEquals(originalManufacturerIdentifier, Long.parseLong(manufacturerIdentifierReference.get()));
        assertEquals(originalOrganizationallyUniqueIdentifier, Integer.parseInt(organizationallyUniqueIdentifierReference.get()));
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_observeSaveData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Intent> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);

        assertNull(saveDataReference.get());
    }

    @Test
    public void test_observeSaveData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent original = new Intent();
        AtomicReference<Intent> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);
        mSavedStateHandle.set("KEY_SAVED_DATA", original);

        assertEquals(original, saveDataReference.get());
    }

    @Test
    public void test_save_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(original).getBytes()
                , -1);
        mViewModel.setModelNumberStringData(Utils.parcelableToByteArray(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(original).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData characteristicData = new CharacteristicData(
                SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(original).getBytes()
                , -1);
        mViewModel.setModelNumberStringData(Utils.parcelableToByteArray(characteristicData));
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00006() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(original).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(characteristicData));
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        mViewModel.setModelNumberStringData(Utils.parcelableToByteArray(modelNumberStringCharacteristicData));

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData));

        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                serviceDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        ServiceData resultServiceData = serviceDataAtomicReference.get();
        assertNotNull(resultServiceData);

        Optional<CharacteristicData> modelNumberStringOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(MODEL_NUMBER_STRING_CHARACTERISTIC))
                .findAny();

        assertTrue(modelNumberStringOptional.isPresent());
        CharacteristicData resultModelNumberStringCharacteristicData = modelNumberStringOptional.get();
        assertEquals(modelNumberStringCharacteristicData, resultModelNumberStringCharacteristicData);

        Optional<CharacteristicData> manufacturerNameStringOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(MANUFACTURER_NAME_STRING_CHARACTERISTIC))
                .findAny();
        assertTrue(manufacturerNameStringOptional.isPresent());
        CharacteristicData resultManufacturerNameStringCharacteristicData = manufacturerNameStringOptional.get();
        assertEquals(manufacturerNameStringCharacteristicData, resultManufacturerNameStringCharacteristicData);
    }

    @Test
    public void test_save_2_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(systemIdCharacteristicData));

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(
                MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        mViewModel.setModelNumberStringData(Utils.parcelableToByteArray(modelNumberStringCharacteristicData));

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData));

        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                serviceDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        ServiceData resultServiceData = serviceDataAtomicReference.get();
        assertNotNull(resultServiceData);

        Optional<CharacteristicData> systemIdOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(SYSTEM_ID_CHARACTERISTIC))
                .findAny();

        assertFalse(systemIdOptional.isPresent());

        Optional<CharacteristicData> modelNumberStringOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(MODEL_NUMBER_STRING_CHARACTERISTIC))
                .findAny();

        assertTrue(modelNumberStringOptional.isPresent());
        CharacteristicData resultModelNumberStringCharacteristicData = modelNumberStringOptional.get();
        assertEquals(modelNumberStringCharacteristicData, resultModelNumberStringCharacteristicData);

        Optional<CharacteristicData> manufacturerNameStringOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(MANUFACTURER_NAME_STRING_CHARACTERISTIC))
                .findAny();
        assertTrue(manufacturerNameStringOptional.isPresent());
        CharacteristicData resultManufacturerNameStringCharacteristicData = manufacturerNameStringOptional.get();
        assertEquals(manufacturerNameStringCharacteristicData, resultManufacturerNameStringCharacteristicData);
    }

    @Test
    public void test_save_2_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(systemIdCharacteristicData));

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        mViewModel.setModelNumberStringData(Utils.parcelableToByteArray(modelNumberStringCharacteristicData));

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(
                MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData));

        mViewModel.updateIsSystemIdSupported(true);
        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                serviceDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        ServiceData resultServiceData = serviceDataAtomicReference.get();
        assertNotNull(resultServiceData);

        Optional<CharacteristicData> systemIdOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(SYSTEM_ID_CHARACTERISTIC))
                .findAny();

        assertTrue(systemIdOptional.isPresent());
        CharacteristicData resultSystemIdCharacteristicData = systemIdOptional.get();
        assertEquals(systemIdCharacteristicData, resultSystemIdCharacteristicData);

        Optional<CharacteristicData> modelNumberStringOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(MODEL_NUMBER_STRING_CHARACTERISTIC))
                .findAny();

        assertTrue(modelNumberStringOptional.isPresent());
        CharacteristicData resultModelNumberStringCharacteristicData = modelNumberStringOptional.get();
        assertEquals(modelNumberStringCharacteristicData, resultModelNumberStringCharacteristicData);

        Optional<CharacteristicData> manufacturerNameStringOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(MANUFACTURER_NAME_STRING_CHARACTERISTIC))
                .findAny();
        assertTrue(manufacturerNameStringOptional.isPresent());
        CharacteristicData resultManufacturerNameStringCharacteristicData = manufacturerNameStringOptional.get();
        assertEquals(manufacturerNameStringCharacteristicData, resultManufacturerNameStringCharacteristicData);
    }

    @Test
    public void test_observeIsSystemIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isSystemIdSupported = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupported::set);

        assertNull(isSystemIdSupported.get());
    }

    @Test
    public void test_observeIsSystemIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isSystemIdSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", original);
        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupported::set);

        assertEquals(original, isSystemIdSupported.get().booleanValue());
    }

    @Test
    public void test_observeIsSystemIdSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isSystemIdSupported = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupported::set);
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", original);

        assertEquals(original, isSystemIdSupported.get().booleanValue());
    }

    @Test
    public void test_observeIsSystemIdSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isSystemIdSupported = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isSystemIdSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", original);
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", original);

        assertEquals(original, isSystemIdSupported.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasSystemIdData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasSystemIdData = new AtomicReference<>();

        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdData::set);

        assertNull(hasSystemIdData.get());
    }

    @Test
    public void test_observeHasSystemIdData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicReference<Boolean> hasSystemIdData = new AtomicReference<>();

        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", original);
        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdData::set);

        assertTrue(hasSystemIdData.get());
    }

    @Test
    public void test_observeHasSystemIdData_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicReference<Boolean> hasSystemIdData = new AtomicReference<>();

        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdData::set);
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", original);

        assertTrue(hasSystemIdData.get());
    }

    @Test
    public void test_observeHasSystemIdData_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasSystemIdData = new AtomicReference<>();

        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasSystemIdData.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", original);
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", original);

        assertTrue(hasSystemIdData.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasSystemIdData_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasSystemIdData = new AtomicReference<>();

        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", original);
        mViewModel.observeHasSystemIdData(new TestLifeCycleOwner(), hasSystemIdData::set);

        assertFalse(hasSystemIdData.get());
    }

    @Test
    public void test_observeHasModelNumberStringData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasModelNumberStringData = new AtomicReference<>();

        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringData::set);

        assertNull(hasModelNumberStringData.get());
    }

    @Test
    public void test_observeHasModelNumberStringData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicReference<Boolean> hasModelNumberStringData = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", original);
        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringData::set);

        assertTrue(hasModelNumberStringData.get());
    }

    @Test
    public void test_observeHasModelNumberStringData_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicReference<Boolean> hasModelNumberStringData = new AtomicReference<>();

        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringData::set);
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", original);

        assertTrue(hasModelNumberStringData.get());
    }

    @Test
    public void test_observeHasModelNumberStringData_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasModelNumberStringData = new AtomicReference<>();

        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasModelNumberStringData.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", original);
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", original);

        assertTrue(hasModelNumberStringData.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasModelNumberStringData_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasModelNumberStringData = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", original);
        mViewModel.observeHasModelNumberStringData(new TestLifeCycleOwner(), hasModelNumberStringData::set);

        assertFalse(hasModelNumberStringData.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasManufacturerNameStringData = new AtomicReference<>();

        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringData::set);

        assertNull(hasManufacturerNameStringData.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicReference<Boolean> hasManufacturerNameStringData = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", original);
        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringData::set);

        assertTrue(hasManufacturerNameStringData.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringData_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicReference<Boolean> hasManufacturerNameStringData = new AtomicReference<>();

        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringData::set);
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", original);

        assertTrue(hasManufacturerNameStringData.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringData_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasManufacturerNameStringData = new AtomicReference<>();

        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasManufacturerNameStringData.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", original);
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", original);

        assertTrue(hasManufacturerNameStringData.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringData_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasManufacturerNameStringData = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", original);
        mViewModel.observeHasManufacturerNameStringData(new TestLifeCycleOwner(), hasManufacturerNameStringData::set);

        assertFalse(hasManufacturerNameStringData.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifier = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifier::set);

        assertNull(manufacturerIdentifier.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> manufacturerIdentifier = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MANUFACTURER_IDENTIFIER", original);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifier::set);

        assertEquals(original, manufacturerIdentifier.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> manufacturerIdentifier = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifier::set);
        mSavedStateHandle.set("KEY_MANUFACTURER_IDENTIFIER", original);

        assertEquals(original, manufacturerIdentifier.get());
    }

    @Test
    public void test_observeManufacturerIdentifier_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> manufacturerIdentifier = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            manufacturerIdentifier.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MANUFACTURER_IDENTIFIER", original);
        mSavedStateHandle.set("KEY_MANUFACTURER_IDENTIFIER", original);

        assertEquals(original, manufacturerIdentifier.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);

        assertNull(organizationallyUniqueIdentifier.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mSavedStateHandle.set("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER", original);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);

        assertEquals(original, organizationallyUniqueIdentifier.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifier::set);
        mSavedStateHandle.set("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER", original);

        assertEquals(original, organizationallyUniqueIdentifier.get());
    }

    @Test
    public void test_observeOrganizationallyUniqueIdentifier_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> organizationallyUniqueIdentifier = new AtomicReference<>();

        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            organizationallyUniqueIdentifier.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER", original);
        mSavedStateHandle.set("KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER", original);

        assertEquals(original, organizationallyUniqueIdentifier.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeModelNumberString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> modelNumberString = new AtomicReference<>();

        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberString::set);

        assertNull(modelNumberString.get());
    }

    @Test
    public void test_observeModelNumberString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> modelNumberString = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING", original);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberString::set);

        assertEquals(original, modelNumberString.get());
    }

    @Test
    public void test_observeModelNumberString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> modelNumberString = new AtomicReference<>();

        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberString::set);
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING", original);

        assertEquals(original, modelNumberString.get());
    }

    @Test
    public void test_observeModelNumberString_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> modelNumberString = new AtomicReference<>();

        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            modelNumberString.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING", original);
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING", original);

        assertEquals(original, modelNumberString.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeManufacturerNameString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerNameString = new AtomicReference<>();

        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameString::set);

        assertNull(manufacturerNameString.get());
    }

    @Test
    public void test_observeManufacturerNameString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> manufacturerNameString = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING", original);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameString::set);

        assertEquals(original, manufacturerNameString.get());
    }

    @Test
    public void test_observeManufacturerNameString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> manufacturerNameString = new AtomicReference<>();

        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameString::set);
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING", original);

        assertEquals(original, manufacturerNameString.get());
    }

    @Test
    public void test_observeManufacturerNameString_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> manufacturerNameString = new AtomicReference<>();

        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            manufacturerNameString.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING", original);
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING", original);

        assertEquals(original, manufacturerNameString.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_getSystemIdData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mViewModel.getSystemIdData());
    }

    @Test
    public void test_getSystemIdData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[1];

        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", original);
        assertArrayEquals(original, mViewModel.getSystemIdData());
    }

    @Test
    public void test_setSystemIdData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        byte[] originalData = Utils.parcelableToByteArray(systemIdCharacteristicData);
        mViewModel.setSystemIdData(originalData);

        assertArrayEquals(originalData, mViewModel.getSystemIdData());
        assertEquals(originalManufacturerIdentifier, Long.parseLong(manufacturerIdentifierReference.get()));
        assertEquals(originalOrganizationallyUniqueIdentifier, Integer.parseInt(organizationallyUniqueIdentifierReference.get()));
    }

    @Test
    public void test_setSystemIdData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        long oldManufacturerIdentifier = 1;
        int oldOrganizationallyUniqueIdentifier = 2;
        CharacteristicData characteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(oldManufacturerIdentifier, oldOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        long originalManufacturerIdentifier = 11;
        int originalOrganizationallyUniqueIdentifier = 22;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(
                SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        byte[] originalData = Utils.parcelableToByteArray(systemIdCharacteristicData);
        mViewModel.setSystemIdData(originalData);

        assertEquals(originalData, mViewModel.getSystemIdData());
        assertEquals(originalManufacturerIdentifier, Long.parseLong(manufacturerIdentifierReference.get()));
        assertEquals(originalOrganizationallyUniqueIdentifier, Integer.parseInt(organizationallyUniqueIdentifierReference.get()));
    }

    @Test
    public void test_setSystemIdData_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        long oldManufacturerIdentifier = 1;
        int oldOrganizationallyUniqueIdentifier = 2;
        CharacteristicData characteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(oldManufacturerIdentifier, oldOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setSystemIdData(null);

        assertNull(mViewModel.getSystemIdData());
        assertNull(manufacturerIdentifierReference.get());
        assertNull(organizationallyUniqueIdentifierReference.get());
    }

    @Test
    public void test_getModelNumberStringData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mViewModel.getModelNumberStringData());
    }

    @Test
    public void test_getModelNumberStringData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[]{1};

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", original);
        assertEquals(original, mViewModel.getModelNumberStringData());
    }

    @Test
    public void test_setModelNumberStringData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        byte[] originalData = Utils.parcelableToByteArray(modelNumberStringCharacteristicData);
        mViewModel.setModelNumberStringData(originalData);

        assertArrayEquals(originalData, mViewModel.getModelNumberStringData());
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
    }

    @Test
    public void test_setModelNumberStringData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(original).getBytes()
                , -1);
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        String originalModelNumberString = "b";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        byte[] originalData = Utils.parcelableToByteArray(modelNumberStringCharacteristicData);
        mViewModel.setModelNumberStringData(originalData);

        assertEquals(originalData, mViewModel.getModelNumberStringData());
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
    }

    @Test
    public void test_setModelNumberStringData_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(original).getBytes()
                , -1);
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setModelNumberStringData(null);

        assertNull(mViewModel.getModelNumberStringData());
        assertNull(modelNumberStringReference.get());
    }

    @Test
    public void test_getManufacturerNameStringData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mViewModel.getManufacturerNameStringData());
    }

    @Test
    public void test_getManufacturerNameStringData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[]{1};

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", original);
        assertArrayEquals(original, mViewModel.getManufacturerNameStringData());
    }


    @Test
    public void test_setManufacturerNameStringData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        byte[] originalData = Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData);
        mViewModel.setManufacturerNameStringData(originalData);

        assertArrayEquals(originalData, mViewModel.getManufacturerNameStringData());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_setManufacturerNameStringData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(original).getBytes()
                , -1);
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        byte[] originalData = Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData);
        mViewModel.setManufacturerNameStringData(originalData);

        assertArrayEquals(originalData, mViewModel.getManufacturerNameStringData());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_setManufacturerNameStringData_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        String original = "a";
        CharacteristicData characteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(original).getBytes()
                , -1);
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setManufacturerNameStringData(null);

        assertNull(mViewModel.getManufacturerNameStringData());
        assertNull(manufacturerNameStringReference.get());
    }

    @Test
    public void test_updateIsSystemIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_IS_SYSTEM_ID_SUPPORTED"));
        mViewModel.updateIsSystemIdSupported(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_SYSTEM_ID_SUPPORTED"));
    }

    @Test
    public void test_updateIsSystemIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateIsSystemIdSupported(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_SYSTEM_ID_SUPPORTED"));

        mViewModel.updateIsSystemIdSupported(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_SYSTEM_ID_SUPPORTED"));
    }

}