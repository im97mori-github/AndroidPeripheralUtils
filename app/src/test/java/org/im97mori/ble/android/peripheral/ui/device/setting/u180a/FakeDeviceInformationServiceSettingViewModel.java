package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeDeviceInformationServiceSettingViewModel extends DeviceInformationServiceSettingViewModel {

    public final PublishProcessor<String> mObserveSetupProcessor = PublishProcessor.create();

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private final SavedStateHandle mSavedStateHandle;

    public final PublishProcessor<Intent> mObserveSaveProcessor = PublishProcessor.create();

    public java.util.function.Consumer<Boolean> mUpdateIsSystemIdSupportedConsumer;

    @Inject
    FakeDeviceInformationServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
        mSavedStateHandle = savedStateHandle;
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupProcessor
                .subscribe(s -> mDisposable.add(Single.<String>create(emitter -> emitter.onSuccess(s))
                        .flatMapCompletable(t -> {
                            switch (t) {
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
                                default:
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
    public void observeSave(@NonNull Consumer<Intent> onSuccess, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveProcessor.subscribe(onSuccess, onError));
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
        mSavedStateHandle.<String>set("KEY_SYSTEM_ID_DATA_JSON", null);
    }

    private void test_systemIdCardView_00002() {
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", "");
    }

    private void test_systemIdSettingButton_00001() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
    }

    private void test_systemIdSettingButton_00002() {
        mSavedStateHandle.set("KEY_IS_SYSTEM_ID_SUPPORTED", true);
        mSavedStateHandle.set("KEY_SYSTEM_ID_DATA_JSON", "a");
    }

    private void test_modelNumberStringCardView_00001() {
        mSavedStateHandle.<String>set("KEY_MODEL_NUMBER_STRING_DATA_JSON", null);
    }

    private void test_modelNumberStringCardView_00002() {
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", "");
    }

    private void test_modelNumberStringSettingButton_00002() {
        mSavedStateHandle.set("KEY_MODEL_NUMBER_STRING_DATA_JSON", "a");
    }

    private void test_manufacturerNameStringCardView_00001() {
        mSavedStateHandle.<String>set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", null);
    }

    private void test_manufacturerNameStringCardView_00002() {
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", "");
    }

    private void test_manufacturerNameStringSettingButton_00002() {
        mSavedStateHandle.set("KEY_MANUFACTURER_NAME_STRING_DATA_JSON", "a");
    }

}