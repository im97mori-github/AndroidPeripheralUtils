package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@HiltViewModel
public class FakeDeviceInformationServiceSettingViewModel extends DeviceInformationServiceSettingViewModel {

    public final PublishSubject<String> mObserveSetupSubject = PublishSubject.create();

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private final SavedStateHandle mSavedStateHandle;

    public final PublishSubject<Intent> mObserveSaveSubject = PublishSubject.create();

    public java.util.function.Consumer<Boolean> mUpdateIsSystemIdSupportedConsumer;

    @Inject
    FakeDeviceInformationServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository) {
        super(savedStateHandle, deviceSettingRepository);
        mSavedStateHandle = savedStateHandle;
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupSubject
                .subscribe(s -> mDisposable.add(Single.<String>create(emitter -> emitter.onSuccess(s))
                        .flatMapCompletable(t -> {
                            switch (t) {
                                // @formatter:off
                                case "test_isSystemIdSupported_00001" : test_isSystemIdSupported_00001(); break;
                                case "test_isSystemIdSupported_00002" : test_isSystemIdSupported_00002(); break;
                                case "test_systemIdCardView_visibility_00002" : test_systemIdCardView_visibility_00002(); break;
                                case "test_systemIdCardView_visibility_00003" : test_systemIdCardView_visibility_00003(); break;
                                case "test_systemIdCardView_00001" : test_systemIdCardView_00001(); break;
                                case "test_systemIdCardView_00002" : test_systemIdCardView_00002(); break;
                                case "test_systemIdSettingButton_00001" : test_systemIdSettingButton_00001(); break;
                                case "test_systemIdSettingButton_00002" : test_systemIdSettingButton_00002(); break;
                                case "test_modelNumberStringCardView_00001" : test_modelNumberStringCardView_00001(); break;
                                case "test_modelNumberStringCardView_00002" : test_modelNumberStringCardView_00002(); break;
                                case "test_modelNumberStringSettingButton_00002" : test_modelNumberStringSettingButton_00002(); break;
                                case "test_manufacturerNameStringCardView_00001" : test_manufacturerNameStringCardView_00001(); break;
                                case "test_manufacturerNameStringCardView_00002" : test_manufacturerNameStringCardView_00002(); break;
                                case "test_manufacturerNameStringSettingButton_00002" : test_manufacturerNameStringSettingButton_00002(); break;
                                case "test_recreate_systemIdCardView_visibility_00001" : test_recreate_systemIdCardView_visibility_00001(); break;
                                case "test_recreate_systemIdCardView_visibility_00002" : test_recreate_systemIdCardView_visibility_00002(); break;
                                case "test_recreate_systemIdCardView_00002" : test_recreate_systemIdCardView_00002(); break;
                                case "test_recreate_modelNumberStringCardView_00002" : test_recreate_modelNumberStringCardView_00002(); break;
                                case "test_recreate_manufacturerNameStringCardView_00002" : test_recreate_manufacturerNameStringCardView_00002(); break;
                                default:
                                // @formatter:on
                            }
                            return Completable.complete();
                        }).subscribe(onComplete, onError))));
    }

    @Override
    public void updateIsSystemIdSupported(boolean checked) {
        if (mUpdateIsSystemIdSupportedConsumer != null) {
            mUpdateIsSystemIdSupportedConsumer.accept(checked);
        }
        super.updateIsSystemIdSupported(checked);
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveSubject.subscribe(intent -> mSavedStateHandle.set("KEY_SAVED_DATA", intent), onError));
    }

    private void test_isSystemIdSupported_00001() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", false);
    }

    private void test_isSystemIdSupported_00002() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
    }

    private void test_systemIdCardView_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", false);
    }

    private void test_systemIdCardView_visibility_00003() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
    }

    private void test_systemIdCardView_00001() {
        mSavedStateHandle.<String>set("KEY_SYSTEM_ID_DATA", null);
    }

    private void test_systemIdCardView_00002() {
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", new byte[]{1});
    }

    private void test_systemIdSettingButton_00001() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
    }

    private void test_systemIdSettingButton_00002() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", new byte[]{1});
    }

    private void test_modelNumberStringCardView_00001() {
        mSavedStateHandle.<String>set("KEY_MODEL_NUMBER_STRING_DATA", null);
    }

    private void test_modelNumberStringCardView_00002() {
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", new byte[]{1});
    }

    private void test_modelNumberStringSettingButton_00002() {
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", new byte[]{1});
    }

    private void test_manufacturerNameStringCardView_00001() {
        mSavedStateHandle.<String>set("KEY_MANUFACTURER_NAME_STRING_DATA", null);
    }

    private void test_manufacturerNameStringCardView_00002() {
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", new byte[]{1});
    }

    private void test_manufacturerNameStringSettingButton_00002() {
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", new byte[]{1});
    }

    private void test_recreate_systemIdCardView_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", false);
    }

    private void test_recreate_systemIdCardView_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
    }

    private void test_recreate_systemIdCardView_00002() {
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA", "");
    }

    private void test_recreate_modelNumberStringCardView_00002() {
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA", new byte[]{1});
    }

    private void test_recreate_manufacturerNameStringCardView_00002() {
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA", new byte[]{1});
    }

}