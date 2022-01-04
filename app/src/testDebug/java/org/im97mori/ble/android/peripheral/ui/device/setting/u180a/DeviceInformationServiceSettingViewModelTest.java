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

import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
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

@SuppressWarnings("ConstantConditions")
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

    @Inject
    Gson mGson;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new DeviceInformationServiceSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository, mGson);
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
        AtomicReference<Boolean> hasSystemIdDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataJsonReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJsonReference::set);
        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJsonReference::set);
        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJsonReference::set);
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
        assertNull(hasSystemIdDataJsonReference.get());
        assertNull(hasModelNumberStringDataJsonReference.get());
        assertNull(hasManufacturerNameStringDataJsonReference.get());
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
        AtomicReference<Boolean> hasSystemIdDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataJsonReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJsonReference::set);
        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJsonReference::set);
        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJsonReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isSystemIdSupportedReference.get());
        assertNull(hasSystemIdDataJsonReference.get());
        assertNull(hasModelNumberStringDataJsonReference.get());
        assertNull(hasManufacturerNameStringDataJsonReference.get());
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
        AtomicReference<Boolean> hasSystemIdDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataJsonReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJsonReference::set);
        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJsonReference::set);
        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJsonReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "a";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        serviceData.characteristicDataList.add(modelNumberStringCharacteristicData);

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "B";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        serviceData.characteristicDataList.add(manufacturerNameStringCharacteristicData);

        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isSystemIdSupportedReference.get());
        assertNull(hasSystemIdDataJsonReference.get());
        assertTrue(hasModelNumberStringDataJsonReference.get());
        assertTrue(hasManufacturerNameStringDataJsonReference.get());
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
        AtomicReference<Boolean> hasSystemIdDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasModelNumberStringDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasManufacturerNameStringDataJsonReference = new AtomicReference<>();
        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();
        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();

        mViewModel.observeIsSystemIdSupported(new TestLifeCycleOwner(), isSystemIdSupportedReference::set);
        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJsonReference::set);
        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJsonReference::set);
        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJsonReference::set);
        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData systemIdCharacteristicData = new CharacteristicData();
        systemIdCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        systemIdCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        systemIdCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        systemIdCharacteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        systemIdCharacteristicData.delay = 1;
        serviceData.characteristicDataList.add(systemIdCharacteristicData);


        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "a";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        serviceData.characteristicDataList.add(modelNumberStringCharacteristicData);

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "B";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        serviceData.characteristicDataList.add(manufacturerNameStringCharacteristicData);

        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isSystemIdSupportedReference.get());
        assertTrue(hasSystemIdDataJsonReference.get());
        assertTrue(hasModelNumberStringDataJsonReference.get());
        assertTrue(hasManufacturerNameStringDataJsonReference.get());
        assertEquals(originalManufacturerIdentifier, Long.parseLong(manufacturerIdentifierReference.get()));
        assertEquals(originalOrganizationallyUniqueIdentifier, Integer.parseInt(organizationallyUniqueIdentifierReference.get()));
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_observeSetup_4_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> observeSetupThrowable = new AtomicReference<>();

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.observeSetup(intent
                , () -> {
                }
                , observeSetupThrowable::set);

        assertEquals("Initialized", observeSetupThrowable.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ModelNumberString(original).getBytes();
        characteristicData.delay = 1;
        mViewModel.setModelNumberStringDataJson(mGson.toJson(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ManufacturerNameString(original).getBytes();
        characteristicData.delay = 1;
        mViewModel.setManufacturerNameStringDataJson(mGson.toJson(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        mViewModel.setSystemIdDataJson(mGson.toJson(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ModelNumberString(original).getBytes();
        characteristicData.delay = 1;
        mViewModel.setModelNumberStringDataJson(mGson.toJson(characteristicData));
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        mViewModel.setSystemIdDataJson(mGson.toJson(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00006() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ManufacturerNameString(original).getBytes();
        characteristicData.delay = 1;
        mViewModel.setManufacturerNameStringDataJson(mGson.toJson(characteristicData));
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        mViewModel.setSystemIdDataJson(mGson.toJson(characteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "a";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        mViewModel.setModelNumberStringDataJson(mGson.toJson(modelNumberStringCharacteristicData));

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "b";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        mViewModel.setManufacturerNameStringDataJson(mGson.toJson(manufacturerNameStringCharacteristicData));

        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> serviceDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.class))
                , throwable -> {
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
    public void test_observeSave_2_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData systemIdCharacteristicData = new CharacteristicData();
        systemIdCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        systemIdCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        systemIdCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        systemIdCharacteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        systemIdCharacteristicData.delay = 1;
        mViewModel.setSystemIdDataJson(mGson.toJson(systemIdCharacteristicData));

        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "a";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        mViewModel.setModelNumberStringDataJson(mGson.toJson(modelNumberStringCharacteristicData));

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "b";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        mViewModel.setManufacturerNameStringDataJson(mGson.toJson(manufacturerNameStringCharacteristicData));

        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> serviceDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.class))
                , throwable -> {
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
    public void test_observeSave_2_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        CharacteristicData systemIdCharacteristicData = new CharacteristicData();
        systemIdCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        systemIdCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        systemIdCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        systemIdCharacteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        systemIdCharacteristicData.delay = 1;
        mViewModel.setSystemIdDataJson(mGson.toJson(systemIdCharacteristicData));

        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "a";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        mViewModel.setModelNumberStringDataJson(mGson.toJson(modelNumberStringCharacteristicData));

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "b";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        mViewModel.setManufacturerNameStringDataJson(mGson.toJson(manufacturerNameStringCharacteristicData));

        mViewModel.updateIsSystemIdSupported(true);
        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> serviceDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.class))
                , throwable -> {
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
    public void test_observeHasSystemIdDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasSystemIdDataJson = new AtomicReference<>();

        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJson::set);

        assertNull(hasSystemIdDataJson.get());
    }

    @Test
    public void test_observeHasSystemIdDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasSystemIdDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", original);
        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJson::set);

        assertTrue(hasSystemIdDataJson.get());
    }

    @Test
    public void test_observeHasSystemIdDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasSystemIdDataJson = new AtomicReference<>();

        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJson::set);
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", original);

        assertTrue(hasSystemIdDataJson.get());
    }

    @Test
    public void test_observeHasSystemIdDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasSystemIdDataJson = new AtomicReference<>();

        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasSystemIdDataJson.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", original);
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", original);

        assertTrue(hasSystemIdDataJson.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasSystemIdDataJson_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasSystemIdDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", original);
        mViewModel.observeHasSystemIdDataJson(new TestLifeCycleOwner(), hasSystemIdDataJson::set);

        assertFalse(hasSystemIdDataJson.get());
    }

    @Test
    public void test_observeHasModelNumberStringDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasModelNumberStringDataJson = new AtomicReference<>();

        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJson::set);

        assertNull(hasModelNumberStringDataJson.get());
    }

    @Test
    public void test_observeHasModelNumberStringDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasModelNumberStringDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", original);
        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJson::set);

        assertTrue(hasModelNumberStringDataJson.get());
    }

    @Test
    public void test_observeHasModelNumberStringDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasModelNumberStringDataJson = new AtomicReference<>();

        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJson::set);
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", original);

        assertTrue(hasModelNumberStringDataJson.get());
    }

    @Test
    public void test_observeHasModelNumberStringDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasModelNumberStringDataJson = new AtomicReference<>();

        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasModelNumberStringDataJson.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", original);
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", original);

        assertTrue(hasModelNumberStringDataJson.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasModelNumberStringDataJson_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasModelNumberStringDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", original);
        mViewModel.observeHasModelNumberStringDataJson(new TestLifeCycleOwner(), hasModelNumberStringDataJson::set);

        assertFalse(hasModelNumberStringDataJson.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasManufacturerNameStringDataJson = new AtomicReference<>();

        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJson::set);

        assertNull(hasManufacturerNameStringDataJson.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasManufacturerNameStringDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", original);
        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJson::set);

        assertTrue(hasManufacturerNameStringDataJson.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasManufacturerNameStringDataJson = new AtomicReference<>();

        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJson::set);
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", original);

        assertTrue(hasManufacturerNameStringDataJson.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasManufacturerNameStringDataJson = new AtomicReference<>();

        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasManufacturerNameStringDataJson.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", original);
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", original);

        assertTrue(hasManufacturerNameStringDataJson.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasManufacturerNameStringDataJson_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasManufacturerNameStringDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", original);
        mViewModel.observeHasManufacturerNameStringDataJson(new TestLifeCycleOwner(), hasManufacturerNameStringDataJson::set);

        assertFalse(hasManufacturerNameStringDataJson.get());
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
    public void test_getSystemIdDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mViewModel.getSystemIdDataJson());
    }

    @Test
    public void test_getSystemIdDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";

        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", original);
        assertEquals(original, mViewModel.getSystemIdDataJson());
    }

    @Test
    public void test_setSystemIdDataJson_00001() {
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

        CharacteristicData systemIdCharacteristicData = new CharacteristicData();
        systemIdCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        systemIdCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        systemIdCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        systemIdCharacteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        systemIdCharacteristicData.delay = 1;
        String originalJson = mGson.toJson(systemIdCharacteristicData);
        mViewModel.setSystemIdDataJson(originalJson);

        assertEquals(originalJson, mViewModel.getSystemIdDataJson());
        assertEquals(originalManufacturerIdentifier, Long.parseLong(manufacturerIdentifierReference.get()));
        assertEquals(originalOrganizationallyUniqueIdentifier, Integer.parseInt(organizationallyUniqueIdentifierReference.get()));
    }

    @Test
    public void test_setSystemIdDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long oldManufacturerIdentifier = 1;
        int oldOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(oldManufacturerIdentifier, oldOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        CharacteristicData systemIdCharacteristicData = new CharacteristicData();
        systemIdCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        systemIdCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        systemIdCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long originalManufacturerIdentifier = 11;
        int originalOrganizationallyUniqueIdentifier = 22;
        systemIdCharacteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        systemIdCharacteristicData.delay = 1;
        String originalJson = mGson.toJson(systemIdCharacteristicData);
        mViewModel.setSystemIdDataJson(originalJson);

        assertEquals(originalJson, mViewModel.getSystemIdDataJson());
        assertEquals(originalManufacturerIdentifier, Long.parseLong(manufacturerIdentifierReference.get()));
        assertEquals(originalOrganizationallyUniqueIdentifier, Integer.parseInt(organizationallyUniqueIdentifierReference.get()));
    }

    @Test
    public void test_setSystemIdDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerIdentifierReference = new AtomicReference<>();
        AtomicReference<String> organizationallyUniqueIdentifierReference = new AtomicReference<>();

        mViewModel.observeManufacturerIdentifier(new TestLifeCycleOwner(), manufacturerIdentifierReference::set);
        mViewModel.observeOrganizationallyUniqueIdentifier(new TestLifeCycleOwner(), organizationallyUniqueIdentifierReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        long oldManufacturerIdentifier = 1;
        int oldOrganizationallyUniqueIdentifier = 2;
        characteristicData.data = new SystemId(oldManufacturerIdentifier, oldOrganizationallyUniqueIdentifier).getBytes();
        characteristicData.delay = 1;
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setSystemIdDataJson(null);

        assertNull(mViewModel.getSystemIdDataJson());
        assertNull(manufacturerIdentifierReference.get());
        assertNull(organizationallyUniqueIdentifierReference.get());
    }

    @Test
    public void test_getModelNumberStringDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mViewModel.getModelNumberStringDataJson());
    }

    @Test
    public void test_getModelNumberStringDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";

        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", original);
        assertEquals(original, mViewModel.getModelNumberStringDataJson());
    }

    @Test
    public void test_setModelNumberStringDataJson_00001() {
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

        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "a";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        String originalJson = mGson.toJson(modelNumberStringCharacteristicData);
        mViewModel.setModelNumberStringDataJson(originalJson);

        assertEquals(originalJson, mViewModel.getModelNumberStringDataJson());
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
    }

    @Test
    public void test_setModelNumberStringDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ModelNumberString(original).getBytes();
        characteristicData.delay = 1;
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData();
        modelNumberStringCharacteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        modelNumberStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        modelNumberStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalModelNumberString = "b";
        modelNumberStringCharacteristicData.data = new ModelNumberString(originalModelNumberString).getBytes();
        modelNumberStringCharacteristicData.delay = 1;
        String originalJson = mGson.toJson(modelNumberStringCharacteristicData);
        mViewModel.setModelNumberStringDataJson(originalJson);

        assertEquals(originalJson, mViewModel.getModelNumberStringDataJson());
        assertEquals(originalModelNumberString, modelNumberStringReference.get());
    }

    @Test
    public void test_setModelNumberStringDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> modelNumberStringReference = new AtomicReference<>();
        mViewModel.observeModelNumberString(new TestLifeCycleOwner(), modelNumberStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MODEL_NUMBER_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ModelNumberString(original).getBytes();
        characteristicData.delay = 1;
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setModelNumberStringDataJson(null);

        assertNull(mViewModel.getModelNumberStringDataJson());
        assertNull(modelNumberStringReference.get());
    }

    @Test
    public void test_getManufacturerNameStringDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mViewModel.getManufacturerNameStringDataJson());
    }

    @Test
    public void test_getManufacturerNameStringDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";

        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", original);
        assertEquals(original, mViewModel.getManufacturerNameStringDataJson());
    }


    @Test
    public void test_setManufacturerNameStringDataJson_00001() {
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

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "b";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        String originalJson = mGson.toJson(manufacturerNameStringCharacteristicData);
        mViewModel.setManufacturerNameStringDataJson(originalJson);

        assertEquals(originalJson, mViewModel.getManufacturerNameStringDataJson());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_setManufacturerNameStringDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ManufacturerNameString(original).getBytes();
        characteristicData.delay = 1;
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData();
        manufacturerNameStringCharacteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        manufacturerNameStringCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        manufacturerNameStringCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String originalManufacturerNameString = "b";
        manufacturerNameStringCharacteristicData.data = new ManufacturerNameString(originalManufacturerNameString).getBytes();
        manufacturerNameStringCharacteristicData.delay = 1;
        String originalJson = mGson.toJson(manufacturerNameStringCharacteristicData);
        mViewModel.setManufacturerNameStringDataJson(originalJson);

        assertEquals(originalJson, mViewModel.getManufacturerNameStringDataJson());
        assertEquals(originalManufacturerNameString, manufacturerNameStringReference.get());
    }

    @Test
    public void test_setManufacturerNameStringDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> manufacturerNameStringReference = new AtomicReference<>();
        mViewModel.observeManufacturerNameString(new TestLifeCycleOwner(), manufacturerNameStringReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = DEVICE_INFORMATION_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = MANUFACTURER_NAME_STRING_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        String original = "a";
        characteristicData.data = new ManufacturerNameString(original).getBytes();
        characteristicData.delay = 1;
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setManufacturerNameStringDataJson(null);

        assertNull(mViewModel.getManufacturerNameStringDataJson());
        assertNull(manufacturerNameStringReference.get());
    }

    @Test
    public void test_updateIsSystemIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        assertNull(mSavedStateHandle.get("KEY_IS_SYSTEM_ID_SUPPORTED"));
        mViewModel.updateIsSystemIdSupported(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_SYSTEM_ID_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateIsSystemIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsSystemIdSupported(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_SYSTEM_ID_SUPPORTED").booleanValue());

        mViewModel.updateIsSystemIdSupported(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_SYSTEM_ID_SUPPORTED").booleanValue());
    }

}