package org.im97mori.ble.android.peripheral.ui.device;

import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.repository.BluetoothSettingRepository;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@SuppressWarnings("ConstantConditions")
@HiltViewModel
public class FakePeripheralViewModel extends PeripheralViewModel {

    public final PublishSubject<String> mObserveSetupSubject = PublishSubject.create();

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private final SavedStateHandle mSavedStateHandle;

    public String test_title_00001_String;

    public boolean mIsQuitCalled;

    public Boolean mIsPeripheralReady;
    public Boolean mIsPeripheralStarted;
    public Boolean mIsBluetoothEnabled;

    public boolean mIsQuitCallSuper = true;
    public boolean mIsStartCalled;

    public boolean mIsObserveDeleteDeviceSettingCalled;

    public boolean mIsBluetoothEnableCalled;
    public boolean mIsBluetoothDisableCalled;

    public boolean mIsClearCalled;

    public boolean mTestActivityResult_00001_1Called;

    @Inject
    FakePeripheralViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository
            , @NonNull BluetoothSettingRepository bluetoothSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, bluetoothSettingRepository, gson);
        mFakeDeviceSettingRepository = deviceSettingRepository;
        mSavedStateHandle = savedStateHandle;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onSuccess, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupSubject
                .subscribe(s -> mDisposable.add(Single.<String>create(emitter -> emitter.onSuccess(s))
                        .flatMapCompletable(t -> {
                            switch (t) {
                                // @formatter:off
                                case "test_title_00001": test_title_00001(); break;
                                case "test_deviceTypeImage_00002": test_deviceTypeImage_00002(); break;
                                case "test_deviceType_00001": test_deviceType_00001(); break;
                                case "test_deviceTypeName_00001": test_deviceTypeName_00001(); break;
                                case "test_observeIsReady_00001": test_observeIsReady_00001(); break;
                                case "test_observeIsStarted_00001": test_observeIsStarted_00001(); break;
                                case "test_observeIsStarted_00002": test_observeIsStarted_00002(); break;
                                case "test_observeIsBluetoothEnabled_00001": test_observeIsBluetoothEnabled_00001(); break;
                                case "test_menu_00001": test_menu_00001(); break;
                                case "test_menu_00002": test_menu_00002(); break;
                                case "test_menu_00003": test_menu_00003(); break;
                                case "test_menu_00004": test_menu_00004(); break;
                                case "test_menu_peripheralStart_00001": test_menu_peripheralStart_00001(); break;
                                case "test_menu_peripheralStop_00001": test_menu_peripheralStop_00001(); break;
                                case "test_menu_setting_00001": test_menu_setting_00001(); break;
                                case "test_menu_bluetooth_enable_00001": test_menu_bluetooth_enable_00001(); break;
                                case "test_bluetooth_disable_00001": test_bluetooth_disable_00001(); break;
                                case "test_activity_result_00001": test_activity_result_00001(); break;
                                case "test_activity_result_00001_1": test_activity_result_00001_1(); break;
                                case "test_activity_result_00002": test_activity_result_00002(); break;
                                case "test_recreate_deviceTypeImage_00002": test_recreate_deviceTypeImage_00002(); break;
                                case "test_recreate_deviceTypeName_00001": test_recreate_deviceTypeName_00001(); break;
                                // @formatter:on
                                default:
                            }
                            return Completable.complete();
                        }).subscribe(onSuccess, onError))));
    }

    @Override
    public boolean isPeripheralReady() {
        if (mIsPeripheralReady == null) {
            return super.isPeripheralReady();
        } else {
            return mIsPeripheralReady;
        }
    }

    @Override
    public synchronized boolean isPeripheralStarted() {
        if (mIsPeripheralStarted == null) {
            return super.isPeripheralStarted();
        } else {
            return mIsPeripheralStarted;
        }
    }

    @Override
    public boolean isBluetoothEnabled() {
        if (mIsBluetoothEnabled == null) {
            return super.isBluetoothEnabled();
        } else {
            return mIsBluetoothEnabled;
        }
    }

    @Override
    public void start() {
        mIsStartCalled = true;
        super.start();
    }

    @Override
    public void quit() {
        mIsQuitCalled = true;
        if (mIsQuitCallSuper) {
            super.quit();
        }
    }

    @Override
    public void clear() {
        mIsClearCalled = true;
        super.clear();
    }

    @Override
    public void bluetoothEnable() {
        mIsBluetoothEnableCalled = true;
    }

    @Override
    public void bluetoothDisable() {
        mIsBluetoothDisableCalled = true;
    }

    @Override
    public void observeDeleteDeviceSetting(@NonNull Intent intent, @NonNull Action onComplete) {
        mIsObserveDeleteDeviceSettingCalled = true;
        super.observeDeleteDeviceSetting(intent, onComplete);
    }

    private void test_title_00001() {
        mSavedStateHandle.set("KEY_TITLE", test_title_00001_String);
    }

    private void test_deviceTypeImage_00002() {
        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", R.drawable.medical_ketsuatsukei_aneroid);
    }

    private void test_deviceType_00001() {
        mSavedStateHandle.set("KEY_DEVICE_TYPE", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
    }

    private void test_deviceTypeName_00001() {
        mSavedStateHandle.set("KEY_DEVICE_TYPE", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
    }

    private void test_observeIsReady_00001() {
        mSavedStateHandle.set("KEY_IS_READY", true);
    }

    private void test_observeIsStarted_00001() {
        mSavedStateHandle.set("KEY_IS_STARTED", true);
    }

    public void test_observeIsStarted_00002() {
        mSavedStateHandle.set("KEY_IS_STARTED", false);
    }

    private void test_observeIsBluetoothEnabled_00001() {
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", true);
    }

    private void test_menu_00001() {
        mIsPeripheralStarted = true;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_menu_00002() {
        mIsPeripheralStarted = false;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_menu_00003() {
        mIsPeripheralStarted = true;
        mIsPeripheralReady = false;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_menu_00004() {
        mIsPeripheralStarted = true;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = false;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_menu_peripheralStart_00001() {
        mIsPeripheralStarted = false;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_menu_peripheralStop_00001() {
        mIsPeripheralStarted = true;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_menu_setting_00001() {
        mIsPeripheralStarted = false;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
        mSavedStateHandle.set("KEY_DEVICE_TYPE", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
    }

    private void test_menu_bluetooth_enable_00001() {
        mIsPeripheralStarted = true;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = false;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_bluetooth_disable_00001() {
        mIsPeripheralStarted = true;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
    }

    private void test_activity_result_00001() {
        mIsPeripheralStarted = false;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
        mSavedStateHandle.set("KEY_DEVICE_TYPE", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
    }

    private void test_activity_result_00001_1() {
        mTestActivityResult_00001_1Called = true;
    }

    private void test_activity_result_00002() {
        mIsPeripheralStarted = false;
        mIsPeripheralReady = true;
        mIsBluetoothEnabled = true;
        mSavedStateHandle.set("KEY_IS_BLUETOOTH_ENABLED", mIsPeripheralReady);
        mSavedStateHandle.set("KEY_IS_READY", mIsPeripheralStarted);
        mSavedStateHandle.set("KEY_IS_STARTED", mIsBluetoothEnabled);
        mSavedStateHandle.set("KEY_DEVICE_TYPE", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
    }

    private void test_recreate_deviceTypeImage_00002() {
        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", R.drawable.medical_ketsuatsukei_aneroid);
    }

    private void test_recreate_deviceTypeName_00001() {
        mSavedStateHandle.set("KEY_DEVICE_TYPE", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
    }

}
